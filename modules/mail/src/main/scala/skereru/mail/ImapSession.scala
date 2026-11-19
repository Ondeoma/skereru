package skereru.mail

import skereru.core.ErrorCode

import cats.effect.IO
import cats.syntax.all.*
import skereru.core.*
import skereru.db.*
import skereru.shared.{Flag, MailText, SpecialUse}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.UUID

/** IMAP のセッション（RFC 3501 + UIDPLUS・MOVE・IDLE・SPECIAL-USE・NAMESPACE・ID・UNSELECT・LITERAL+・SASL-IR）。 */
final class ImapSession(
  conn: Conn,
  cfg: MailConfig,
  tls: Option[TlsHolder],
  b: Bridge,
  mail: MailStore,
  blobs: skereru.storage.Blobs,
  queue: QueueStore,
  auth: MailAuth,
  log: String => Unit) {

  private var user: Option[User] = None
  private var sel: Option[Selected] = None
  private var open = true
  private val SystemFlags = List(Flag.Answered, Flag.Flagged, Flag.Deleted, Flag.Seen, Flag.Draft)

  private def out(
    s: String
  ): Unit = conn.write(s + "\r\n")

  private def capabilities: String = {
    val base = List(
      "IMAP4rev1",
      "LITERAL+",
      "SASL-IR",
      "ID",
      "IDLE",
      "NAMESPACE",
      "UIDPLUS",
      "MOVE",
      "UNSELECT",
      "SPECIAL-USE",
      "CHILDREN",
      "ENABLE"
    )
    val tlsCaps = if !conn.isTls && tls.isDefined then List("STARTTLS") else Nil
    val authCaps =
      if user.isDefined then Nil
      else if conn.isTls || cfg.allowPlainAuth then List("AUTH=PLAIN")
      else List("LOGINDISABLED")
    (base ++ tlsCaps ++ authCaps).mkString(" ")
  }

  def run(): Unit = {
    conn.setTimeout(60_000)
    out(s"* OK [CAPABILITY $capabilities] ${cfg.hostname} IMAP4rev1 skereru ready")
    while open do {
      readCommand() match {
        case None => open = false
        case Some(Left(msg)) => out(s"* BAD $msg")
        case Some(Right(bytes)) =>
          val r = new ImapReader(bytes)
          var tag = "*"
          try {
            tag = r.tag()
            r.sp()
            val cmd = r.atom().toUpperCase
            handle(tag, cmd, r)
          } catch {
            case e: ImapSyntax => out(s"$tag BAD ${e.getMessage}")
            case e: java.io.IOException => throw e
            case e: Throwable =>
              log(s"[imap] ${user.map(_.id).getOrElse("-")}: $e")
              out(s"$tag NO [SERVERBUG] internal error")
          }
      }
      conn.setTimeout(if user.isDefined then 30 * 60 * 1000 else 60_000)
    }
  }

  /** コマンドを1つ読む（リテラルを含めて組み立てる）。 */
  private def readCommand(): Option[Either[String, Array[Byte]]] = {
    val outBuf = new ByteArrayOutputStream()
    // ログインの前は小さなリテラルしか受け付けない（認証なしでメモリを使わせない）
    val limit = if user.isDefined then cfg.maxMessageBytes + 64 * 1024 else 16L * 1024
    var done = false
    while !done do {
      val line =
        try conn.readLineBytes(64 * 1024)
        catch case _: LineTooLong => return Some(Left("line too long"))
      line match {
        case None => return None
        case Some(l) =>
          outBuf.write(l)
          val lit = """\{(\d+)(\+?)\}$""".r.findFirstMatchIn(new String(l, StandardCharsets.ISO_8859_1))
          lit match {
            case None => done = true
            case Some(m) =>
              val n = m.group(1).toLongOption.getOrElse(Long.MaxValue)
              if n > limit - outBuf.size() then // n + size だと、巨大な n で桁あふれする
                if m.group(2).isEmpty then return Some(Left("literal too large"))
                else { open = false; out("* BYE literal too large"); return None }
              if m.group(2).isEmpty then out("+ Ready for literal data")
              outBuf.write("\r\n".getBytes)
              conn.readBytes(n.toInt) match {
                case None => return None
                case Some(data) => outBuf.write(data)
              }
          }
      }
    }
    Some(Right(outBuf.toByteArray))
  }

  private def needAuth(
    tag: String
  )(
    f: User => Unit
  ): Unit = user match {
    case None => out(s"$tag BAD Please log in first")
    case Some(u) => f(u)
  }

  private def needSelected(
    tag: String
  )(
    f: (User, Selected) => Unit
  ): Unit = (user, sel) match {
    case (Some(u), Some(s)) => f(u, s)
    case (None, _) => out(s"$tag BAD Please log in first")
    case _ => out(s"$tag BAD No mailbox selected")
  }

  private def handle(
    tag: String,
    cmd: String,
    r: ImapReader
  ): Unit = cmd match {
    case "CAPABILITY" => r.end(); out(s"* CAPABILITY $capabilities"); out(s"$tag OK CAPABILITY completed")
    case "NOOP" => r.end(); poll(expunges = true); out(s"$tag OK NOOP completed")
    case "LOGOUT" => r.end(); out(s"* BYE ${cfg.hostname} logging out"); out(s"$tag OK LOGOUT completed"); open = false
    case "ID" => out("* ID (\"name\" \"skereru\" \"version\" \"" + cfg.version + "\")"); out(s"$tag OK ID completed")
    case "STARTTLS" => starttls(tag, r)
    case "LOGIN" => login(tag, r)
    case "AUTHENTICATE" => authenticate(tag, r)
    case "ENABLE" => needAuth(tag) { _ => out("* ENABLED"); out(s"$tag OK ENABLE completed") }
    case "NAMESPACE" => needAuth(tag) { _ =>
        r.end(); out("* NAMESPACE ((\"\" \"/\")) NIL NIL"); out(s"$tag OK NAMESPACE completed")
      }
    case "SELECT" | "EXAMINE" => needAuth(tag)(u => select(tag, u, r, cmd == "EXAMINE"))
    case "CREATE" => needAuth(tag)(u => create(tag, u, r))
    case "DELETE" => needAuth(tag)(u => delete(tag, u, r))
    case "RENAME" => needAuth(tag)(u => rename(tag, u, r))
    case "SUBSCRIBE" | "UNSUBSCRIBE" => needAuth(tag) { u =>
        r.sp(); val name = r.mailbox(); r.end()
        b.run(mail.subscribe(u.id, name, cmd == "SUBSCRIBE"))
        out(s"$tag OK $cmd completed")
      }
    case "LIST" | "LSUB" => needAuth(tag)(u => list(tag, u, r, cmd == "LSUB"))
    case "STATUS" => needAuth(tag)(u => status(tag, u, r))
    case "APPEND" => needAuth(tag)(u => append(tag, u, r))
    case "IDLE" => needAuth(tag)(_ => idle(tag))
    case "CHECK" => needSelected(tag) {
        (
          _,
          _
        ) => poll(expunges = true); out(s"$tag OK CHECK completed")
      }
    case "CLOSE" => needSelected(tag) {
        (
          _,
          s
        ) =>
          if !s.readOnly then b.run(mail.expunge(s.box.id, None))
          sel = None
          out(s"$tag OK CLOSE completed")
      }
    case "UNSELECT" => needSelected(tag) {
        (
          _,
          _
        ) => sel = None; out(s"$tag OK UNSELECT completed")
      }
    case "EXPUNGE" => needSelected(tag)(
        (
          _,
          s
        ) => expunge(tag, s, None)
      )
    case "SEARCH" => needSelected(tag)(
        (
          _,
          s
        ) => search(tag, s, r, uid = false)
      )
    case "FETCH" => needSelected(tag)(
        (
          _,
          s
        ) => fetch(tag, s, r, uid = false)
      )
    case "STORE" => needSelected(tag)(
        (
          _,
          s
        ) => store(tag, s, r, uid = false)
      )
    case "COPY" | "MOVE" => needSelected(tag)(
        (
          u,
          s
        ) => copy(tag, u, s, r, uid = false, move = cmd == "MOVE")
      )
    case "UID" => needSelected(tag) {
        (
          u,
          s
        ) =>
          r.sp()
          r.atom().toUpperCase match {
            case "FETCH" => fetch(tag, s, r, uid = true)
            case "SEARCH" => search(tag, s, r, uid = true)
            case "STORE" => store(tag, s, r, uid = true)
            case "COPY" => copy(tag, u, s, r, uid = true, move = false)
            case "MOVE" => copy(tag, u, s, r, uid = true, move = true)
            case "EXPUNGE" => r.sp(); val set = r.seqSet(); r.end(); expunge(tag, s, Some(set))
            case other => out(s"$tag BAD Unknown UID command $other")
          }
      }
    case other => out(s"$tag BAD Unknown command $other")
  }

  // ---------------------------------------------------------------- 認証

  private def starttls(
    tag: String,
    r: ImapReader
  ): Unit = {
    r.end()
    if conn.isTls then out(s"$tag BAD TLS already active")
    else
      tls match {
        case None => out(s"$tag NO TLS not available")
        case Some(t) =>
          out(s"$tag OK Begin TLS negotiation now")
          val ip = conn.clientIp
          conn.startTls(t.context) // 読み込み済みの平文（STARTTLS インジェクション）は捨てる
          conn.clientIp = ip
      }
  }

  private def doLogin(
    tag: String,
    login: String,
    pass: String
  ): Unit =
    if user.isDefined then out(s"$tag BAD Already authenticated")
    else if !conn.isTls && !cfg.allowPlainAuth then out(s"$tag NO [PRIVACYREQUIRED] Use STARTTLS or the TLS port first")
    else
      b.run(auth.login(login, pass, conn.clientIp, "imap")) match {
        case Right(u) =>
          user = Some(u)
          conn.deadline = Long.MaxValue
          b.run(mail.ensureDefaults(u.id))
          out(s"$tag OK [CAPABILITY $capabilities] Logged in")
        case Left(_) => out(s"$tag NO [AUTHENTICATIONFAILED] Authentication failed")
      }

  private def login(
    tag: String,
    r: ImapReader
  ): Unit = {
    r.sp(); val u = r.astring(); r.sp(); val p = r.astring(); r.end()
    doLogin(tag, u, p)
  }

  private def authenticate(
    tag: String,
    r: ImapReader
  ): Unit = {
    r.sp()
    val mech = r.atom().toUpperCase
    if mech != "PLAIN" then { out(s"$tag NO [CANNOT] Unsupported mechanism"); return }
    if !conn.isTls && !cfg.allowPlainAuth then { out(s"$tag NO [PRIVACYREQUIRED] Use STARTTLS first"); return }
    val initial = if r.hasMore then { r.sp(); Some(r.atom("=")) }
    else None
    val resp = initial.orElse { out("+ "); conn.readLine(4096) }
    resp.filterNot(_ == "*") match {
      case None => out(s"$tag BAD Authentication cancelled")
      case Some(b64) =>
        scala.util.Try(new String(
          java.util.Base64.getDecoder.decode(b64.trim),
          StandardCharsets.UTF_8
        )).toOption.map(_.split('\u0000')) match {
          case Some(Array(_, u, p)) => doLogin(tag, u, p)
          case _ => out(s"$tag BAD Invalid response")
        }
    }
  }

  // ---------------------------------------------------------------- メールボックス

  private def validName(
    n: String
  ): Boolean = MailText.validMailboxName(n)

  private def select(
    tag: String,
    u: User,
    r: ImapReader,
    readOnly: Boolean
  ): Unit = {
    r.sp(); val name = r.mailbox()
    sel = None
    b.run(mail.mailboxByName(u.id, name)) match {
      case None => out(s"$tag NO [NONEXISTENT] Mailbox does not exist")
      case Some(box) =>
        val s = new Selected(box, readOnly)
        val rows = b.run(mail.uidFlags(box.id))
        rows.foreach { x => s.uids += x.uid; s.flags(x.uid) = x.flags }
        s.modseq = rows.map(_.modseq).maxOption.fold(box.highestModseq)(m => math.max(m, box.highestModseq))
        sel = Some(s)
        out("* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft $Forwarded $Junk $NotJunk)")
        out(if readOnly then "* OK [PERMANENTFLAGS ()] Read-only mailbox"
        else "* OK [PERMANENTFLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft \\*)] Flags permitted")
        out(s"* ${s.exists} EXISTS")
        out("* 0 RECENT")
        rows.indexWhere(x => !x.flags.contains(Flag.Seen)) match {
          case -1 => ()
          case i => out(s"* OK [UNSEEN ${i + 1}] First unseen")
        }
        out(s"* OK [UIDVALIDITY ${box.uidvalidity}] UIDs valid")
        out(s"* OK [UIDNEXT ${box.uidnext}] Predicted next UID")
        out(s"$tag OK [${if readOnly then "READ-ONLY" else "READ-WRITE"}] ${
            if readOnly then "EXAMINE" else "SELECT"
          } completed")
    }
  }

  private def create(
    tag: String,
    u: User,
    r: ImapReader
  ): Unit = {
    r.sp(); val name = r.mailbox().stripSuffix("/"); r.end()
    if !validName(name) then out(s"$tag NO [CANNOT] Invalid mailbox name")
    else {
      // 途中の階層（a/b を作るときの a）も作る
      val parents = name.split('/').inits.toList.reverse.drop(1).dropRight(1).map(_.mkString("/"))
      parents.foreach(p => if b.run(mail.mailboxByName(u.id, p)).isEmpty then b.run(mail.createMailbox(u.id, p)))
      b.run(mail.createMailbox(u.id, name)) match {
        case Right(_) => out(s"$tag OK CREATE completed")
        case Left(_) => out(s"$tag NO [ALREADYEXISTS] Mailbox already exists")
      }
    }
  }

  private def delete(
    tag: String,
    u: User,
    r: ImapReader
  ): Unit = {
    r.sp(); val name = r.mailbox(); r.end()
    if sel.exists(_.box.name == name) then sel = None
    b.run(mail.deleteMailbox(u.id, name)) match {
      case Right(_) => out(s"$tag OK DELETE completed")
      case Left(e) if e.code == ErrorCode.MailboxNotFound => out(s"$tag NO [NONEXISTENT] Mailbox does not exist")
      case Left(e) => out(s"$tag NO [CANNOT] ${ascii(e.message)}")
    }
  }

  private def rename(
    tag: String,
    u: User,
    r: ImapReader
  ): Unit = {
    r.sp(); val from = r.mailbox(); r.sp(); val to = r.mailbox(); r.end()
    if !validName(to) then out(s"$tag NO [CANNOT] Invalid mailbox name")
    else
      b.run(mail.renameMailbox(u.id, from, to)) match {
        case Right(_) => out(s"$tag OK RENAME completed")
        case Left(e) if e.code == ErrorCode.MailboxNotFound => out(s"$tag NO [NONEXISTENT] Mailbox does not exist")
        case Left(e) if e.code == ErrorCode.MailboxAlreadyExists =>
          out(s"$tag NO [ALREADYEXISTS] Target mailbox exists")
        case Left(e) => out(s"$tag NO [CANNOT] ${ascii(e.message)}")
      }
  }

  /** 応答の文言（日本語のメッセージは IMAP では送れないので英語にする）。 */
  private def ascii(
    s: String
  ): String = if s.forall(_ < 0x80) then s else "Operation not permitted"

  private def matches(
    pattern: String,
    name: String
  ): Boolean =
    glob(pattern, name) || (name == "INBOX" && glob(pattern.toUpperCase, "INBOX"))

  /** LIST のパターン（* は何でも、% は "/" 以外）の照合。正規表現だと * を並べたパターンで計算が爆発するので、表で O(パターン×名前) にする。 */
  private def glob(
    p: String,
    n: String
  ): Boolean =
    if p.length > 1000 then false
    else {
      // ok(j): パターンの先頭 i 文字が名前の先頭 j 文字に合う
      var ok = Array.tabulate(n.length + 1)(_ == 0)
      p.foreach { c =>
        val next = new Array[Boolean](n.length + 1)
        c match {
          case '*' =>
            var any = false
            for j <- 0 to n.length do { any = any || ok(j); next(j) = any }
          case '%' =>
            var any = false
            for j <- 0 to n.length do {
              if j > 0 && n(j - 1) == '/' then any = false
              any = any || ok(j); next(j) = any
            }
          case _ =>
            for j <- 1 to n.length do next(j) = ok(j - 1) && n(j - 1) == c
        }
        ok = next
      }
      ok(n.length)
    }

  private def list(
    tag: String,
    u: User,
    r: ImapReader,
    lsub: Boolean
  ): Unit = {
    r.sp()
    // LIST-EXTENDED の選択肢（"(SPECIAL-USE)" など）は読み飛ばす
    if r.peekChar == '(' then { r.flagList(); r.sp() }
    val ref = r.astring(); r.sp()
    val pat0 = r.listMailbox()
    val pattern = MUtf7.decode(ref + pat0)
    while r.hasMore do { r.sp(); if r.peekChar == '(' then r.flagList() else r.atom() }
    if pat0.isEmpty then
      out(s"""* ${if lsub then "LSUB" else "LIST"} (\\Noselect) "/" """"")
    else {
      val boxes = b.run(mail.mailboxes(u.id))
      val names = boxes.map(_.name).toSet
      val sorted = boxes.sortBy(x => (x.name != "INBOX", x.name.toLowerCase))
      sorted.filter(x => matches(pattern, x.name) && (!lsub || x.subscribed)).foreach { x =>
        val kids = names.exists(_.startsWith(x.name + "/"))
        val attrs = List(if kids then "\\HasChildren" else "\\HasNoChildren") ++
          x.specialUse.flatMap(SpecialUse.imapAttr).toList
        out(s"""* ${if lsub then "LSUB" else "LIST"} (${attrs.mkString(" ")}) "/" ${ImapFormat.mailbox(x.name)}""")
      }
    }
    out(s"$tag OK ${if lsub then "LSUB" else "LIST"} completed")
  }

  private def status(
    tag: String,
    u: User,
    r: ImapReader
  ): Unit = {
    r.sp(); val name = r.mailbox(); r.sp()
    val items = r.flagList().map(_.toUpperCase)
    r.end()
    b.run(mail.mailboxByName(u.id, name)) match {
      case None => out(s"$tag NO [NONEXISTENT] Mailbox does not exist")
      case Some(box) =>
        val d = b.run(mail.mailboxDtos(u.id)).find(_.id == box.id)
        val vals = items.flatMap {
          case "MESSAGES" => Some(s"MESSAGES ${d.map(_.total).getOrElse(0)}")
          case "RECENT" => Some("RECENT 0")
          case "UIDNEXT" => Some(s"UIDNEXT ${box.uidnext}")
          case "UIDVALIDITY" => Some(s"UIDVALIDITY ${box.uidvalidity}")
          case "UNSEEN" => Some(s"UNSEEN ${d.map(_.unread).getOrElse(0)}")
          case _ => None
        }
        out(s"* STATUS ${ImapFormat.mailbox(box.name)} (${vals.mkString(" ")})")
        out(s"$tag OK STATUS completed")
    }
  }

  private def append(
    tag: String,
    u: User,
    r: ImapReader
  ): Unit = {
    r.sp(); val name = r.mailbox(); r.sp()
    val flags = if r.peekChar == '(' then { val f = r.flagList(); r.sp(); f }
    else Nil
    val date = if r.peekChar == '"' then { val d = r.string(); r.sp(); ImapFormat.parseInternalDate(d) }
    else None
    val raw = Compose.crlf(r.astringBytes())
    r.end()
    b.run(mail.mailboxByName(u.id, name)) match {
      case None => out(s"$tag NO [TRYCREATE] Mailbox does not exist")
      case Some(box) =>
        val (quota, used) = b.run(mail.quota(u.id, cfg.defaultQuotaBytes))
        if raw.length > cfg.maxMessageBytes then out(s"$tag NO [TOOBIG] Message too large")
        else if used + raw.length > quota then out(s"$tag NO [OVERQUOTA] Quota exceeded")
        else {
          val okFlags = cleanFlags(flags)
          val id = b.run(blobs.put(raw))
          val m = b.run(mail.insert(
            u.id,
            box.id,
            Indexer.index(raw, id, date.getOrElse(java.time.Instant.now()), okFlags, None, None)
          ))
          if sel.exists(_.box.id == box.id) then poll(expunges = true)
          out(s"$tag OK [APPENDUID ${box.uidvalidity} ${m.uid}] APPEND completed")
        }
    }
  }

  private def cleanFlags(
    fs: List[String]
  ): List[String] =
    fs.flatMap { f =>
      SystemFlags.find(
        _.equalsIgnoreCase(f)
      ).orElse(Option.when(!f.startsWith("\\") && f.nonEmpty && f.length <= 100)(f))
    }.distinct

  // ---------------------------------------------------------------- 変化の通知

  /** 選択中のメールボックスの変化（追加・削除・フラグ）を知らせる。expunges=false なら EXPUNGE は後回しにする。 */
  private def poll(
    expunges: Boolean
  ): Unit = sel.foreach { s =>
    b.run(mail.mailbox(s.box.id)) match {
      case None =>
        out("* BYE Mailbox has been deleted"); open = false
      case Some(box) if box.highestModseq != s.modseq =>
        val known = s.maxUid
        // 消えたものがあるか（知っている範囲の件数が減ったか）を数えて、減っていたときだけ UID の並びを読む
        val missing = s.uids.nonEmpty && b.run(mail.countUpTo(box.id, known)) != s.uids.size
        if missing && expunges then {
          val now = b.run(mail.uidsUpTo(box.id, known)).toSet
          // 大きい番号から知らせると、残りの番号はずれない
          s.uids.filterNot(now.contains).toList.map(u => s.seqOf(u)).filter(_ > 0).sorted.reverse.foreach { seq =>
            out(s"* $seq EXPUNGE")
            val uid = s.uids(seq - 1)
            s.uids.remove(seq - 1)
            s.flags.remove(uid)
          }
        }
        // 追加・フラグの変更は MODSEQ が進んだものだけを読む
        val changed = b.run(mail.changedSince(box.id, s.modseq))
        val added = changed.filter(_.uid > known)
        if added.nonEmpty then {
          added.foreach { x => s.uids += x.uid; s.flags(x.uid) = x.flags }
          out(s"* ${s.exists} EXISTS")
        }
        changed.filter(x => x.uid <= known && s.flags.get(x.uid).exists(_ != x.flags)).foreach { x =>
          s.flags(x.uid) = x.flags
          val seq = s.seqOf(x.uid)
          if seq > 0 then out(s"* $seq FETCH (FLAGS (${x.flags.mkString(" ")}) UID ${x.uid})")
        }
        // 知らせていない消去が残っていれば、次の確認でもう一度見る
        if !missing || expunges then s.modseq = box.highestModseq
      case _ => ()
    }
  }

  private def idle(
    tag: String
  ): Unit = {
    out("+ idling")
    conn.setTimeout(cfg.imapIdlePollMs)
    val started = System.currentTimeMillis()
    var waiting = true
    while waiting && open do
      try
        conn.readLine(1024) match {
          case None => open = false; waiting = false
          case Some(l) =>
            waiting = false
            if l.trim.equalsIgnoreCase("DONE") then out(s"$tag OK IDLE terminated")
            else out(s"$tag BAD Expected DONE")
        }
      catch {
        case _: java.net.SocketTimeoutException =>
          poll(expunges = true)
          if System.currentTimeMillis() - started > 29 * 60 * 1000 then {
            out("* BYE IDLE timed out"); open = false; waiting = false
          }
      }
    conn.setTimeout(30 * 60 * 1000)
  }

  // ---------------------------------------------------------------- メッセージ

  /** シーケンス集合（または UID 集合）を、選択中のメールボックスの UID にする。 */
  private def resolve(
    s: Selected,
    set: SeqSet,
    uid: Boolean
  ): List[Long] =
    if uid then
      // UID の範囲は、存在する UID のうち範囲に入るもの（"*" は最大の UID）
      s.uids.filter(u => set.contains(u, s.maxUid)).toList
    else
      (1 to s.exists).filter(i => set.contains(i.toLong, s.exists.toLong)).map(i => s.uids(i - 1)).toList

  private def expunge(
    tag: String,
    s: Selected,
    only: Option[SeqSet]
  ): Unit =
    if s.readOnly then out(s"$tag NO [READ-ONLY] Mailbox is read-only")
    else {
      val uids = only.map(set => resolve(s, set, uid = true))
      b.run(mail.expunge(s.box.id, uids))
      poll(expunges = true)
      out(s"$tag OK EXPUNGE completed")
    }

  private def search(
    tag: String,
    s: Selected,
    r: ImapReader,
    uid: Boolean
  ): Unit = {
    r.sp()
    // CHARSET の指定（UTF-8 と US-ASCII だけ）
    val mark = r.position
    val key0 =
      if r.peekChar.toUpper == 'C' && r.atom().equalsIgnoreCase("CHARSET") then {
        r.sp()
        val cs = r.astring().toUpperCase
        if cs != "UTF-8" && cs != "US-ASCII" then {
          out(s"$tag NO [BADCHARSET (UTF-8 US-ASCII)] Unsupported charset"); return
        }
        r.sp()
        r.searchKeys()
      } else {
        r.rewind(mark)
        r.searchKeys()
      }
    val key = toUidKeys(s, key0)
    val found = b.run(mail.search(s.box.id, key, s.maxUid, cfg.zone)).filter(u => s.seqOf(u) > 0)
    val nums = if uid then found else found.map(s.seqOf(_).toLong)
    out(("* SEARCH" :: nums.sorted.map(_.toString)).mkString(" "))
    out(s"$tag OK SEARCH completed")
  }

  /** シーケンス番号の条件を UID の条件に置き換える。 */
  private def toUidKeys(
    s: Selected,
    k: SearchKey
  ): SearchKey = k match {
    case SearchKey.Seq(set) =>
      val uids = resolve(s, set, uid = false)
      if uids.isEmpty then SearchKey.Not(SearchKey.All) else SearchKey.Uid(SeqSet.parse(SeqSet.format(uids)))
    case SearchKey.Not(x) => SearchKey.Not(toUidKeys(s, x))
    case SearchKey.Or(a, c) => SearchKey.Or(toUidKeys(s, a), toUidKeys(s, c))
    case SearchKey.And(ks) => SearchKey.And(ks.map(toUidKeys(s, _)))
    case other => other
  }

  private def fetch(
    tag: String,
    s: Selected,
    r: ImapReader,
    uid: Boolean
  ): Unit = {
    r.sp()
    val set = r.seqSet(); r.sp()
    val atts0 = r.fetchAtts()
    // CHANGEDSINCE などの修飾子は読み飛ばす
    while r.hasMore do { r.sp(); if r.peekChar == '(' then r.flagList() else r.atom() }
    val atts = if uid && !atts0.contains(FetchAtt.Uid) then FetchAtt.Uid :: atts0 else atts0
    val uids = resolve(s, set, uid)
    if uids.isEmpty then { out(s"$tag OK FETCH completed"); return }
    val needBody = atts.exists {
      case FetchAtt.Flags | FetchAtt.Uid | FetchAtt.InternalDate | FetchAtt.Rfc822Size | FetchAtt.ModSeq => false
      case _ => true
    }
    val setSeen = !s.readOnly && atts.exists(_.setsSeen)
    // 大きなメールボックスでも一度に読み過ぎないよう、500件ずつ処理する
    uids.grouped(500).foreach { chunk =>
      val metas = b.run(mail.byUids(s.box.id, chunk)).map(m => m.uid -> m).toMap
      val seenNow =
        if setSeen then {
          val unseen = chunk.filter(u => metas.get(u).exists(!_.flags.contains(Flag.Seen)))
          b.run(mail.storeFlags(s.box.id, unseen, "add", List(Flag.Seen))).map(x => x.uid -> x).toMap
        } else Map.empty[Long, UidFlags]
      chunk.foreach { u =>
        metas.get(u).foreach { m =>
          val flags = seenNow.get(u).map(_.flags).getOrElse(m.flags)
          s.flags(u) = flags
          val seq = s.seqOf(u)
          val raw =
            if needBody then b.run(blobs.get(m.blobId)).getOrElse(Array.emptyByteArray) else Array.emptyByteArray
          lazy val root = Mime.parse(raw)
          val buf = new ByteArrayOutputStream()
          def w(
            x: String
          ): Unit = buf.write(x.getBytes(StandardCharsets.ISO_8859_1))
          def wu(
            x: String
          ): Unit = buf.write(x.getBytes(StandardCharsets.UTF_8))
          def lit(
            bytes: Array[Byte]
          ): Unit = { w(s"{${bytes.length}}\r\n"); buf.write(bytes) }
          w(s"* $seq FETCH (")
          val items = (if seenNow.contains(u) && !atts.contains(FetchAtt.Flags) then FetchAtt.Flags :: atts
                       else atts).distinct
          items.zipWithIndex.foreach {
            (
              a,
              i
            ) =>
              {
                if i > 0 then w(" ")
                a match {
                  case FetchAtt.Flags => w(s"FLAGS (${flags.mkString(" ")})")
                  case FetchAtt.Uid => w(s"UID $u")
                  case FetchAtt.InternalDate =>
                    w(s"INTERNALDATE ${ImapFormat.internalDate(m.internalDate, cfg.zone)}")
                  case FetchAtt.Rfc822Size => w(s"RFC822.SIZE ${m.size}")
                  case FetchAtt.ModSeq => w(s"MODSEQ (${m.modseq})")
                  case FetchAtt.Envelope => w("ENVELOPE "); w(ImapFormat.envelope(root))
                  case FetchAtt.BodyStructure =>
                    w("BODYSTRUCTURE "); w(ImapFormat.bodyStructure(raw, root, extended = true))
                  case FetchAtt.Body => w("BODY "); w(ImapFormat.bodyStructure(raw, root, extended = false))
                  case FetchAtt.Rfc822 => w("RFC822 "); lit(raw)
                  case FetchAtt.Rfc822Header =>
                    w("RFC822.HEADER "); lit(ImapFormat.sectionBytes(raw, root, Section(Nil, "HEADER", Nil)))
                  case FetchAtt.Rfc822Text =>
                    w("RFC822.TEXT "); lit(ImapFormat.sectionBytes(raw, root, Section(Nil, "TEXT", Nil)))
                  case FetchAtt.BodySection(_, sec, partial) =>
                    val data = ImapFormat.sectionBytes(raw, root, sec)
                    partial match {
                      case None => wu(s"BODY[${sec.render}] "); lit(data)
                      case Some((start, len)) =>
                        val from = math.min(start, data.length.toLong).toInt
                        val to = math.min(start + len, data.length.toLong).toInt
                        wu(s"BODY[${sec.render}]<$start> "); lit(java.util.Arrays.copyOfRange(data, from, to))
                    }
                }
              }
          }
          w(")\r\n")
          conn.writeBytes(buf.toByteArray)
        }
      }
      conn.flush()
    }
    out(s"$tag OK ${if uid then "UID " else ""}FETCH completed")
  }

  private def store(
    tag: String,
    s: Selected,
    r: ImapReader,
    uid: Boolean
  ): Unit = {
    r.sp()
    val set = r.seqSet(); r.sp()
    // UNCHANGEDSINCE（CONDSTORE）は使わない
    if r.peekChar == '(' then { r.flagList(); r.sp() }
    val item = r.atom().toUpperCase
    r.sp()
    val flags = cleanFlags(r.flagList())
    r.end()
    if s.readOnly then { out(s"$tag NO [READ-ONLY] Mailbox is read-only"); return }
    val mode = item.stripSuffix(".SILENT") match {
      case "FLAGS" => "set"
      case "+FLAGS" => "add"
      case "-FLAGS" => "remove"
      case _ => throw ImapSyntax("invalid STORE item")
    }
    val silent = item.endsWith(".SILENT")
    val uids = resolve(s, set, uid)
    val changed = b.run(mail.storeFlags(s.box.id, uids, mode, flags))
    // \Junk・$Junk を付けたものは迷惑メールとして学習する
    if mode != "remove" && flags.exists(f => f.equalsIgnoreCase(Flag.Junk)) then
      train(s.box.userId, changed.map(_.uid), s.box.id, spam = true)
    if mode != "remove" && flags.exists(f => f.equalsIgnoreCase(Flag.NotJunk)) then
      train(s.box.userId, changed.map(_.uid), s.box.id, spam = false)
    val byUid = changed.map(c => c.uid -> c).toMap
    uids.foreach { u =>
      val f = byUid.get(u).map(_.flags).orElse(s.flags.get(u)).getOrElse(Nil)
      val before = s.flags.get(u)
      s.flags(u) = f
      if !silent || before.isEmpty then {
        val seq = s.seqOf(u)
        if seq > 0 then out(s"* $seq FETCH (FLAGS (${f.mkString(" ")})${if uid then s" UID $u" else ""})")
      }
    }
    out(s"$tag OK STORE completed")
  }

  private def copy(
    tag: String,
    u: User,
    s: Selected,
    r: ImapReader,
    uid: Boolean,
    move: Boolean
  ): Unit = {
    r.sp()
    val set = r.seqSet(); r.sp()
    val name = r.mailbox()
    r.end()
    if move && s.readOnly then { out(s"$tag NO [READ-ONLY] Mailbox is read-only"); return }
    b.run(mail.mailboxByName(u.id, name)) match {
      case None => out(s"$tag NO [TRYCREATE] Mailbox does not exist")
      case Some(dest) =>
        val uids = resolve(s, set, uid)
        val metas = b.run(mail.byUids(s.box.id, uids))
        val (quota, used) = b.run(mail.quota(u.id, cfg.defaultQuotaBytes))
        if !move && used + metas.map(_.size.toLong).sum > quota then out(s"$tag NO [OVERQUOTA] Quota exceeded")
        else {
          val pairs = b.run(mail.copy(s.box.id, uids, dest.id, move))
          // 迷惑メールへ移した・迷惑メールから戻したものを学習する
          if dest.specialUse.contains(SpecialUse.Junk) && !s.box.specialUse.contains(SpecialUse.Junk) then
            train(u.id, pairs.map(_._2), dest.id, spam = true)
          else if s.box.specialUse.contains(SpecialUse.Junk) &&
            !dest.specialUse.exists(x => x == SpecialUse.Trash || x == SpecialUse.Junk)
          then
            train(u.id, pairs.map(_._2), dest.id, spam = false)
          val code = if pairs.isEmpty then ""
          else s"[COPYUID ${dest.uidvalidity} ${SeqSet.format(pairs.map(_._1))} ${SeqSet.format(pairs.map(_._2))}] "
          if move then {
            if code.nonEmpty then out(s"* OK $code")
            poll(expunges = true)
            out(s"$tag OK MOVE completed")
          } else out(s"$tag OK ${code}COPY completed")
          if sel.exists(_.box.id == dest.id) then poll(expunges = false)
        }
    }
  }

  /** 迷惑メールの学習（応答を待たせないよう、別のスレッドで）。 */
  private def train(
    userId: UUID,
    uids: List[Long],
    boxId: UUID,
    spam: Boolean
  ): Unit =
    if uids.nonEmpty then
      Thread.ofVirtual().start { () =>
        try
          b.run(mail.byUids(boxId, uids).flatMap(_.take(50).traverse_ { m =>
            blobs.get(m.blobId).flatMap(_.traverse_(raw =>
              queue.train(userId, Bayes.tokens(raw, Mime.parse(raw)), spam, undo = false)
            ))
          }))
        catch case e: Throwable => log(s"[imap] train: $e")
      }
}
