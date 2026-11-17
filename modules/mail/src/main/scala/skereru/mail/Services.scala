package skereru.mail

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import skereru.auth.AppPasswords
import skereru.core.*
import skereru.db.*
import skereru.shared.*

import java.io.{DataInputStream, DataOutputStream}
import java.net.{InetAddress, Socket}
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.UUID

/** メッセージを解析して、保存する属性（件名・差出人・宛先・プレビュー・検索用の本文）を作る。 */
object Indexer:

  def bodyText(raw: Array[Byte], root: MimePart): String =
    val leaves = root.leaves().map(_._2)
    val plain = leaves.find(p => p.mimeType == "text/plain" && !p.isAttachment)
    val html  = leaves.find(p => p.mimeType == "text/html" && !p.isAttachment)
    plain.map(Mime.text(raw, _)).orElse(html.map(h => Html.toText(Mime.text(raw, h)))).getOrElse("")

  def index(raw: Array[Byte], blobId: String, internalDate: Instant, flags: List[String], spam: Option[SpamVerdict], auth: Option[Json],
      parsed: Option[MimePart] = None): NewMessage =
    val root = parsed.getOrElse(Mime.parse(raw))
    val from = root.header("From").flatMap(Address.parseOne)
    // 表示名は引用符で囲んで保存する（"Doe, John" のようにカンマを含む名前があるため）
    def addrs(h: String) = root.header(h).toList.flatMap(Address.parseList)
      .map(a => MailText.quoted(a.nameDecoded, a.address)).mkString(", ")
    val text = bodyText(raw, root)
    val files = root.leaves().map(_._2).filter(_.isAttachment)
    val refs = root.header("References").toList.flatMap(r => "<[^>]+>".r.findAllIn(r).toList)
    NewMessage(
      blobId = blobId,
      size = raw.length,
      internalDate = internalDate,
      flags = flags,
      subject = root.headerDecoded("Subject").getOrElse("").replaceAll("[\r\n\t]+", " ").trim,
      fromAddr = from.map(_.address.toLowerCase),
      fromName = from.flatMap(_.nameDecoded),
      toAddrs = addrs("To"),
      ccAddrs = addrs("Cc"),
      sentAt = root.header("Date").flatMap(Dates.parseRfc5322),
      messageId = root.header("Message-ID").map(_.trim).filter(_.nonEmpty).map(_.take(500)),
      inReplyTo = root.header("In-Reply-To").flatMap(r => "<[^>]+>".r.findFirstIn(r)),
      references = refs.takeRight(30),
      hasAttachments = files.nonEmpty,
      preview = text.replaceAll("(?m)^>.*$", "").replaceAll("\\s+", " ").trim.take(200),
      // 添付ファイル（テキスト・PDF・Office・ODF）の本文も検索できるようにする
      searchText = (text.take(500_000) + "\n" + files.flatMap(_.filename).mkString(" ") +
        (if files.isEmpty then "" else "\n" + AttachmentText.extract(raw, root))).replace("\u0000", ""),
      spamScore = spam.map(_.score),
      spamReport = spam.map(_.summary),
      authResults = auth
    )

/** メールソフト（IMAP・POP3・SMTP）のログイン。アプリパスワードで認証し、総当たりはロックする。 */
final class MailAuth(users: UserStore):
  private val ipMax = 20       // 1つの IP から（どのアカウントでも）
  private val pairMax = 8      // 1つの IP から1つのアカウントへ

  private val windowSecs = 15 * 60
  private val lockSecs = 15 * 60

  def login(login: String, password: String, ip: InetAddress, proto: String): IO[Either[String, User]] =
    // IPv6 は /64 単位で数える（/64 の中でアドレスを変えて、ロックを回避させない）
    val ipKey = s"ip:${IpKey.of(ip)}"
    val pairKey = s"pair:${IpKey.of(ip)}:${login.trim.toLowerCase}"
    val now = Instant.now()
    // アカウント全体ではロックしない（ほかの人が失敗を重ねて、正しい利用者を締め出せないように）。
    // アプリパスワードは約75ビットあるので、分散した総当たりでも現実的には当たらない
    (users.lockedUntil(ipKey, now), users.lockedUntil(pairKey, now)).tupled.flatMap {
      case (Some(_), _) | (_, Some(_)) => IO.sleep(scala.concurrent.duration.DurationInt(1).second).as(Left("認証の失敗が多いため、しばらくログインできません"))
      case _ =>
        if login.trim.isEmpty || password.isEmpty || login.length > 320 || password.length > 200 then IO.pure(Left("ユーザー名かパスワードが違います"))
        else
          users.checkAppPassword(login.trim, AppPasswords.hash(password), ip.getHostAddress).flatMap {
            case Some((u, _)) => users.clearFailures(pairKey).as(Right(u))
            case None =>
              for
                l1 <- users.recordFailure(ipKey, ipMax, windowSecs, lockSecs)
                l3 <- users.recordFailure(pairKey, pairMax, windowSecs, lockSecs)
                _  <- users.audit(None, proto, "mail.login.failed", Some(login.take(200)), Some(ip.getHostAddress))
                _  <- IO.whenA(l1 || l3)(users.audit(None, proto, "mail.login.locked", Some(login.take(200)), Some(ip.getHostAddress)))
                // 失敗の応答を遅らせて、総当たりを遅くする
                _  <- IO.sleep(scala.concurrent.duration.DurationInt(1).second)
              yield Left("ユーザー名かパスワードが違います")
          }
    }

/** ClamAV（clamd の INSTREAM）。 */
final class ClamAv(host: String, port: Int):
  /** ウイルスの名前（見つからなければ None）。つながらなければ例外。 */
  def scan(data: Array[Byte]): Option[String] =
    val s = new Socket(host, port)
    try
      s.setSoTimeout(60_000)
      val out = new DataOutputStream(s.getOutputStream)
      out.write("zINSTREAM\u0000".getBytes(StandardCharsets.US_ASCII))
      data.grouped(64 * 1024).foreach { c => out.writeInt(c.length); out.write(c) }
      out.writeInt(0)
      out.flush()
      val reply = new String(new DataInputStream(s.getInputStream).readAllBytes(), StandardCharsets.US_ASCII).replace("\u0000", "").trim
      if reply.endsWith("OK") then None
      else if reply.endsWith("FOUND") then Some(reply.stripPrefix("stream:").stripSuffix("FOUND").trim)
      else throw new RuntimeException(s"clamd: $reply")
    finally s.close()

/** 受信したメッセージの迷惑メール判定（送信ドメイン認証・DNSBL・ベイズ・ウイルス・規則）。 */
final class SpamFilter(dns: JndiDns, queue: QueueStore, cfg: MailConfig, log: String => Unit):
  private val spf   = new Spf(dns)
  private val dkim  = new Dkim(dns)
  private val dmarc = new Dmarc(dns)
  private val clam  = cfg.clamav.map((h, p) => new ClamAv(h, p))

  final case class Result(verdict: SpamVerdict, auth: AuthResultDto, header: String, json: Json)

  def check(raw: Array[Byte], root: MimePart, ip: InetAddress, helo: String, mailFrom: String, bayesUser: Option[UUID],
      isLocalDomain: String => Boolean): IO[Result] =
    val fromDomain = root.header("From").flatMap(Address.parseOne).map(_.domain).getOrElse("")
    val mfDomain = if mailFrom.isEmpty then helo else Address.domainOf(mailFrom)
    // 互いに関係しない問い合わせは並列にする（DMARC だけは SPF・DKIM の結果を使う）
    val authIO = (IO.blocking(spf.check(ip, mailFrom, helo)), IO.blocking(dkim.verify(raw, Instant.now()))).parTupled.flatMap { (spfRes, dkimRes) =>
      IO.blocking(if fromDomain.isEmpty then DmarcResult("none", "none", "") else dmarc.evaluate(fromDomain, spfRes, mfDomain, dkimRes))
        .map(d => (spfRes, dkimRes, d))
    }
    val rdnsIO = IO.blocking(dns.verifiedReverse(ip))
    val blIO = IO.blocking(if ip.isLoopbackAddress || ip.isSiteLocalAddress then Nil else dns.dnsbl(ip, cfg.dnsblZones))
    val virusIO = IO.blocking(clam.flatMap(c => scala.util.Try(c.scan(raw)).fold(e => { log(s"[spam] clamd: $e"); None }, identity)))
    (authIO, rdnsIO, blIO, virusIO).parTupled.flatMap { case ((spfRes, dkimRes, dmarcRes), rdns, bl, virus) =>
      val toks = Bayes.tokens(raw, root)
      bayesUser.traverse(u => queue.bayesCounts(u, toks)).map(_.flatMap((c, s, h) => skereru.core.Bayes.classify(c, s, h))).map { bayes =>
        val in = SpamInput(raw, root, Some(ip), helo, rdns, mailFrom, spfRes, dkimRes, dmarcRes, bl, bayes,
          // From のどれか1つでもこのサーバーのドメインなら、名乗っているとみなす
          claimsLocalDomain = SpamRules.fromAddresses(root).exists(a => a.domain.nonEmpty && isLocalDomain(a.domain)), virus = virus, now = Instant.now())
        val v = SpamRules.evaluate(in, cfg.thresholds)
        val dkimStr = dkimRes.find(_.pass).map(r => s"pass (d=${r.domain})").orElse(dkimRes.headOption.map(r => s"${r.result} (d=${r.domain})")).getOrElse("none")
        val auth = AuthResultDto(s"${spfRes.label} (${if mailFrom.isEmpty then "helo=" + helo else "smtp.mailfrom=" + mailFrom})", dkimStr,
          s"${dmarcRes.result} (p=${dmarcRes.policy} header.from=$fromDomain)")
        val header = s"Authentication-Results: ${cfg.hostname};\r\n\tspf=${spfRes.label} smtp.mailfrom=${if mailFrom.isEmpty then "<>" else mailFrom};\r\n\t" +
          (if dkimRes.isEmpty then "dkim=none" else dkimRes.map(r => s"dkim=${r.result} header.d=${r.domain} header.s=${r.selector}").mkString(";\r\n\t")) +
          s";\r\n\tdmarc=${dmarcRes.result} (p=${dmarcRes.policy}) header.from=$fromDomain\r\n"
        Result(v, auth, header, auth.asJson)
      }
    }

/** 宛先の解決結果（配送するユーザーと、外への転送先）。 */
final case class Target(users: List[UUID], forwards: List[String], dmarcReports: Boolean = false)

/** 受信したメッセージ（エンベロープ）。 */
final case class Envelope(ip: InetAddress, helo: String, mailFrom: String, rcpts: List[String], tls: Boolean, authUser: Option[UUID])

enum InboundResult:
  case Accepted(id: String)
  case Rejected(code: String, msg: String)
  case TempFail(code: String, msg: String)

/** 配送（受け取ったメッセージを宛先のメールボックスに入れる・外へ送る）。 */
final class Delivery(mail: MailStore, domains: DomainStore, users: UserStore, queue: QueueStore, blobs: skereru.storage.Blobs, spam: SpamFilter,
    signer: Signer, cfg: MailConfig, log: String => Unit, notify: UUID => IO[Unit]):

  /** DMARC の集計レポートを受け取ったとき（起動時に保存先をつなぐ）。 */
  @volatile var onDmarcReport: (Array[Byte], Boolean) => IO[Unit] = (_, _) => IO.unit

  /** アドレスが配送先として存在するか（+tag・キャッチオールも見る）。転送先は再帰的にたどる。 */
  def resolve(rcpt: String, depth: Int = 0): IO[Option[Target]] =
    val a = rcpt.toLowerCase
    domains.address(a).flatMap {
      case Some(r) => Some(r).pure[IO]
      case None    => if Address.withoutTag(a) != a then domains.address(Address.withoutTag(a)) else IO.pure(None)
    }.flatMap {
      case Some(r) =>
        // 転送先のうち、このサーバーのアドレスはたどって配送先のユーザーにする
        r.forwardTo.traverse { f =>
          if depth >= 5 then IO.pure(Target(Nil, Nil))
          else domains.isLocalDomain(Address.domainOf(f)).flatMap {
            case true  => resolve(f, depth + 1).map(_.getOrElse(Target(Nil, Nil)))
            case false => IO.pure(Target(Nil, List(f)))
          }
        }.map { ts => Some(Target((r.userId.toList ++ ts.flatMap(_.users)).distinct, ts.flatMap(_.forwards).distinct)) }
      case None =>
        // DMARC の集計レポートの受け口（アドレスを作っていなくても受け取る）
        if Address.localOf(a) == Delivery.DmarcReportsLocal && isLocal(Address.domainOf(a)) then IO.pure(Some(Target(Nil, Nil, dmarcReports = true)))
        else domains.domainByName(Address.domainOf(a)).map(_.flatMap(_.catchAll).map(u => Target(List(u), Nil)))
    }

  def isLocalDomain(d: String): IO[Boolean] = domains.isLocalDomain(d)

  /** 受信の前に足すヘッダー。 */
  private def received(env: Envelope, rdns: Option[String], proto: String, forRcpt: Option[String]): String =
    val from = s"${env.helo.take(100).replaceAll("[^A-Za-z0-9.\\-\\[\\]:]", "")} (${rdns.getOrElse("unknown")} [${env.ip.getHostAddress}])"
    s"Received: from $from\r\n\tby ${cfg.hostname} (skereru) with $proto\r\n\t${forRcpt.fold("")(r => s"for <$r>")}; ${Dates.rfc5322(Instant.now(), cfg.zone)}\r\n"

  /** 外から受け取ったメッセージ（ポート25）。迷惑メールの判定をして、宛先のユーザーに配る。 */
  def inbound(env: Envelope, raw0: Array[Byte]): IO[InboundResult] =
    val root0 = Mime.parse(raw0)
    // 転送が回り続けているメール（Received が多すぎる）は受け取らない
    if root0.headerAll("Received").size > Delivery.MaxHops then
      return IO.pure(InboundResult.Rejected("554 5.4.6", "Too many hops (mail loop detected)"))
    for
      targets <- env.rcpts.traverse(r => resolve(r).map(r -> _))
      userIds  = targets.flatMap(_._2.toList.flatMap(_.users)).distinct
      res <- spam.check(raw0, root0, env.ip, env.helo, env.mailFrom, userIds.headOption, d => isLocal(d))
      out <- res.verdict.action match
        case SpamAction.Reject =>
          queue.log("in", "rejected", Some(env.ip.getHostAddress), env.mailFrom, env.rcpts, raw0.length, Some(res.verdict.score), res.verdict.summary)
            .as(InboundResult.Rejected("550 5.7.1", s"Message rejected as spam (score ${res.verdict.score})"))
        case action =>
          val proto = if env.tls then "ESMTPS" else "ESMTP"
          val spamHeaders =
            s"X-Spam-Score: ${res.verdict.score}\r\nX-Spam-Status: ${if action == SpamAction.Junk then "Yes" else "No"}, score=${res.verdict.score} tests=${res.verdict.hits.map(_.rule).mkString(",")}\r\n"
          val head = s"Return-Path: <${env.mailFrom}>\r\n" + received(env, None, proto, env.rcpts.headOption.filter(_ => env.rcpts.size == 1)) + res.header + spamHeaders
          val raw = head.getBytes(StandardCharsets.ISO_8859_1) ++ stripSpoofable(raw0)
          for
            id <- blobs.put(raw)
            results <- userIds.traverse(u => deliverToUsers(List(u), raw, id, Some(res), env.mailFrom, junk = action == SpamAction.Junk)).map(_.flatten)
            fwds = targets.flatMap(_._2.toList.flatMap(_.forwards)).distinct
            // DMARC の集計レポートは、解析して保存する（受信トレイには入れない）。認証されたかどうかも残す
            _ <- IO.whenA(targets.exists(_._2.exists(_.dmarcReports)))(onDmarcReport(raw0, res.auth.spf.startsWith("pass") || res.auth.dkim.startsWith("pass"))
                   .handleErrorWith(e => IO(log(s"[dmarc] report: $e"))))
            _ <- IO.whenA(fwds.nonEmpty && action != SpamAction.Junk)(forward(env.rcpts.headOption.getOrElse(""), env.mailFrom, fwds, raw, Some(res.header)))
            // 誰も受け取れなかった（容量の上限など）なら、黙って捨てずに一時エラーにして、相手に再送・通知してもらう
            allFailed = results.nonEmpty && results.forall(_._2.isLeft) && fwds.isEmpty
            _ <- queue.log("in", if allFailed then "tempfail" else if action == SpamAction.Junk then "junk" else "delivered", Some(env.ip.getHostAddress),
                   env.mailFrom, env.rcpts, raw.length, Some(res.verdict.score), if allFailed then results.flatMap(_._2.left.toOption).mkString(" | ") else res.verdict.summary)
          yield
            if allFailed then InboundResult.TempFail("452 4.2.2", "Mailbox full or unavailable")
            else InboundResult.Accepted(id.take(12))
    yield out

  // 判定に使うローカルのドメインの一覧（毎回 DB を引かないよう同期的に持つ）
  @volatile private var localDomains: Set[String] = Set.empty
  def refreshDomains: IO[Unit] = domains.domains.map(ds => localDomains = ds.filter(_.enabled).map(_.name).toSet)
  private def isLocal(d: String): Boolean = localDomains.contains(d.toLowerCase)

  /** 外から来たメッセージにあると紛らわしいヘッダー（自分が付ける判定結果）を取り除く。 */
  private def stripSpoofable(raw: Array[Byte]): Array[Byte] =
    val bad = Set("x-spam-score", "x-spam-status", "x-skereru-auth")
    // 自分のホスト名を名乗る Authentication-Results も取り除く（RFC 8601 5章）
    RawHeaders.filter(raw)(h => !(bad(h.name.toLowerCase) ||
      (h.name.equalsIgnoreCase("Authentication-Results") && h.raw.drop(h.raw.indexOf(':') + 1).trim.toLowerCase.startsWith(cfg.hostname.toLowerCase))))

  /** ユーザーのメールボックスに入れる（許可・拒否の差出人、振り分けの規則、自動返信、容量）。 */
  def deliverToUsers(userIds: List[UUID], raw: Array[Byte], blobId: String, spamRes: Option[SpamFilter#Result], mailFrom: String, junk: Boolean,
      envelopeTo: List[String] = Nil): IO[List[(UUID, Either[String, UUID])]] =
    val root = Mime.parse(raw)
    val from = root.header("From").flatMap(Address.parseOne).map(_.address.toLowerCase).getOrElse(mailFrom.toLowerCase)
    userIds.distinct.traverse { uid =>
      (for
        u <- users.find(uid)
        settings <- users.settings(uid)
        qu <- mail.quota(uid, cfg.defaultQuotaBytes)
        (quota, used) = qu
        r <- u match
          case None => IO.pure(Left("ユーザーがいません"))
          case Some(user) if user.disabled => IO.pure(Left("無効なユーザー"))
          case Some(_) if used + raw.length > quota => IO.pure(Left("容量の上限を超えています"))
          case Some(_) =>
            val listed = (l: List[String]) => l.map(_.trim.toLowerCase).filter(_.nonEmpty).exists(x => from == x || (x.startsWith("@") && from.endsWith(x)) || from.endsWith("@" + x))
            val blocked = listed(settings.blockSenders)
            val allowed = listed(settings.allowSenders)
            for
              rules <- queue.rules(uid)
              o = Rules.apply(rules, raw, root, envelopeTo)
              isJunk = (junk || blocked) && !allowed && !o.notSpam
              box <- (if isJunk then mail.special(uid, SpecialUse.Junk)
                      else if o.trash then mail.special(uid, SpecialUse.Trash)
                      else o.moveTo.flatTraverse(n => mail.mailboxByName(uid, n)).flatMap(_.fold(mail.special(uid, SpecialUse.Inbox))(b => IO.pure(Some(b)))))
              _ <- IO.whenA(box.isEmpty)(mail.ensureDefaults(uid))
              box2 <- box.fold(mail.special(uid, SpecialUse.Inbox))(b => IO.pure(Some(b)))
              flags = (if o.markRead then List(Flag.Seen) else Nil) ++ (if o.flag then List(Flag.Flagged) else Nil) ++ (if isJunk then List(Flag.Junk) else Nil)
              meta = Indexer.index(raw, blobId, Instant.now(), flags, spamRes.map(_.verdict), spamRes.map(_.json), Some(root))
              m <- mail.insert(uid, box2.get.id, meta)
              _ <- notify(uid)
              _ <- IO.whenA(o.forwards.nonEmpty && !isJunk)(ruleForward(uid, mailFrom, o.forwards, raw, spamRes.map(_.header)).handleErrorWith(e => IO(log(s"[deliver] forward: $e"))))
              // 自動返信は、返信先（エンベロープの差出人）が認証されたメールにだけ返す（偽の差出人への返信＝バックスキャッターを防ぐ）。
              // SPF はエンベロープの差出人のドメインで調べている。DKIM は署名のドメインが返信先と同じ組織のときだけ認める
              authOk = spamRes.forall(r => r.auth.spf.startsWith("pass") || DkimPass.findFirstMatchIn(r.auth.dkim).exists(m =>
                OrgDomain.of(m.group(1)) == OrgDomain.of(Address.domainOf(mailFrom))))
              _ <- IO.whenA(!isJunk && authOk && settings.vacation.enabled)(vacation(uid, settings, root, mailFrom).handleErrorWith(e => IO(log(s"[deliver] vacation: $e"))))
              _ <- users.touchContacts(uid, root.header("From").flatMap(Address.parseOne).toList.map(a => a.address -> a.nameDecoded)).whenA(!isJunk)
            yield Right(m.id)
      yield r).handleErrorWith(e => IO(log(s"[deliver] $uid: $e")).as(Left(e.getMessage))).map(uid -> _)
    }

  /** 振り分けの規則の転送（ユーザーの1時間の送信数の上限に含める。乗っ取られたアカウントの踏み台にさせない）。 */
  private def ruleForward(uid: UUID, mailFrom: String, to: List[String], raw: Array[Byte], auth: Option[String]): IO[Unit] =
    queue.recentRecipients(uid).flatMap { n =>
      if n + to.size > cfg.maxRecipientsPerHour then IO(log(s"[deliver] forward skipped: user $uid is over the hourly limit"))
      else
        // エンベロープの差出人は、転送するユーザー自身のアドレスにする（元の差出人のままだと転送先で SPF が通らず、
        // 拒否の通知が元の差出人に届いてしまう＝バックスキャッター）。bounce は空のまま
        if mailFrom.isEmpty then blobs.put(raw).flatMap(id => queue.enqueue(Some(uid), "", to.take(20), id)).void
        else domains.sendable(uid).flatMap {
          case own :: _ =>
            signer.arcSeal(raw, Address.domainOf(own), auth, cfg.hostname).flatMap(withArc =>
              blobs.put(withArc).flatMap(id => queue.enqueue(Some(uid), own, to.take(20), id))).void
          case Nil      => IO(log(s"[deliver] forward skipped: user $uid has no address"))
        }
    }

  /** 転送（エンベロープの差出人は転送したアドレス。bounce は空のまま）。 */
  private def forward(via: String, mailFrom: String, to: List[String], raw: Array[Byte], auth: Option[String] = None): IO[Unit] =
    val sender = if mailFrom.isEmpty then "" else via
    signer.arcSeal(raw, Address.domainOf(via), auth, cfg.hostname).flatMap(withArc =>
      blobs.put(withArc).flatMap(id => queue.enqueue(None, sender, to.take(20), id))).void

  private val DkimPass = """^pass \(d=([^)\s]+)\)""".r

  /** 不在の自動返信（同じ相手には4日に1回。自動送信・メーリングリスト・bounce には返さない）。 */
  private def vacation(uid: UUID, s: MailSettingsDto, root: MimePart, mailFrom: String): IO[Unit] =
    val v = s.vacation
    val today = java.time.LocalDate.now(cfg.zone).toString
    val inPeriod = v.startDate.forall(_ <= today) && v.endDate.forall(_ >= today)
    val auto = root.header("Auto-Submitted").exists(!_.trim.equalsIgnoreCase("no")) || root.header("List-Id").isDefined ||
      root.header("Precedence").exists(p => Set("bulk", "list", "junk")(p.trim.toLowerCase)) || mailFrom.isEmpty ||
      mailFrom.toLowerCase.matches("^(mailer-daemon|postmaster|noreply|no-reply|donotreply)@.*")
    if !inPeriod || auto then IO.unit
    else
      (domains.primaryAddress(uid), users.find(uid)).tupled.flatMap {
        case (Some(me), Some(u)) =>
          val sendIt = if v.contactsOnly then users.contacts(uid, mailFrom, 1).map(_.exists(_.address == mailFrom.toLowerCase)) else IO.pure(true)
          sendIt.flatMap(ok => if !ok then IO.unit else queue.vacationOnce(uid, mailFrom, 4).flatMap {
            case false => IO.unit
            case true =>
              val o = Outgoing(MailAddress(Some(u.name), Address.localOf(me), Address.domainOf(me)), List(MailAddress(None, Address.localOf(mailFrom), Address.domainOf(mailFrom))),
                Nil, if v.subject.trim.nonEmpty then v.subject else "不在のお知らせ", v.body, None, Nil, root.header("Message-ID"), root.header("Message-ID").toList,
                List("Auto-Submitted" -> "auto-replied", "Precedence" -> "bulk"))
              val raw = Compose.crlf(Compose.build(o, Compose.newMessageId(Address.domainOf(me)), Instant.now(), cfg.zone))
              signer.sign(raw).flatMap(signed => blobs.put(signed).flatMap(id => queue.enqueue(Some(uid), "", List(mailFrom), id))).void
          })
        case _ => IO.unit
      }

  /** 認証したユーザーが送る（SMTP の submission・画面・API）。From は自分のアドレスに限る。
    * 戻り値は (このサーバーの宛先に配った数, 外への送信待ちにした数)。 */
  def submit(user: User, mailFrom: String, rcpts: List[String], raw0: Array[Byte], via: String, ip: Option[String]): IO[Either[String, (Int, Int)]] =
    val root = Mime.parse(raw0)
    val fromAddrs = root.header("From").toList.flatMap(Address.parseList).map(_.address.toLowerCase)
    for
      sendable <- domains.sendable(user.id).map(_.toSet)
      recent   <- queue.recentRecipients(user.id)
      res <- (
        if fromAddrs.isEmpty then IO.pure(Left("From がありません"))
        else if root.headerAll("From").size > 1 || root.headerAll("Sender").size > 1 then IO.pure(Left("From・Sender は1つにしてください"))
        else if root.header("Sender").flatMap(Address.parseOne).exists(a => !sendable.contains(a.address.toLowerCase)) then
          IO.pure(Left("Sender のアドレスは使えません。自分のアドレスにしてください"))
        else if !fromAddrs.forall(sendable.contains) then IO.pure(Left(s"From のアドレス（${fromAddrs.mkString(", ")}）は使えません。自分のアドレスにしてください"))
        else if mailFrom.nonEmpty && !sendable.contains(mailFrom.toLowerCase) then IO.pure(Left(s"MAIL FROM のアドレス（$mailFrom）は使えません"))
        else if rcpts.isEmpty then IO.pure(Left("宛先がありません"))
        else if recent + rcpts.size > cfg.maxRecipientsPerHour then IO.pure(Left(s"送信できる宛先の数の上限（1時間に ${cfg.maxRecipientsPerHour}）を超えています"))
        else
          val envFrom = if mailFrom.nonEmpty then mailFrom.toLowerCase else fromAddrs.head
          // Date・Message-ID がなければ付ける。Bcc は外へ出さない
          val extra = (if root.header("Date").isEmpty then s"Date: ${Dates.rfc5322(Instant.now(), cfg.zone)}\r\n" else "") +
            (if root.header("Message-ID").isEmpty then s"Message-ID: ${Compose.newMessageId(Address.domainOf(envFrom))}\r\n" else "")
          val noBcc = removeHeader(Compose.crlf(raw0), "Bcc")
          val withHead = (s"Received: by ${cfg.hostname} (skereru) with ${via}; ${Dates.rfc5322(Instant.now(), cfg.zone)}\r\n" + extra).getBytes(StandardCharsets.ISO_8859_1) ++ noBcc
          for
            signed <- signer.sign(withHead)
            id <- blobs.put(signed)
            split <- rcpts.distinct.traverse(r => isLocalDomain(Address.domainOf(r)).map(r -> _))
            (local, remote) = split.partitionMap((r, l) => if l then Left(r) else Right(r))
            targets <- local.traverse(r => resolve(r).map(r -> _))
            missing = targets.collect { case (r, None) => r }
            delivered <- targets.flatMap(_._2.toList).traverse(t => deliverToUsers(t.users, signed, id, None, envFrom, junk = false, envelopeTo = local)).map(_.flatten.count(_._2.isRight))
            fwds = targets.flatMap(_._2.toList.flatMap(_.forwards)).distinct
            queued <- queue.enqueue(Some(user.id), envFrom, (remote ++ fwds).distinct, id)
            // このサーバーの存在しない宛先は、すぐに差出人へ知らせる
            _ <- IO.whenA(missing.nonEmpty)(bounceToSender(envFrom, signed, missing.map(_ -> "550 5.1.1 宛先のアドレスがありません")))
            _ <- users.touchContacts(user.id, (root.header("To").toList ++ root.header("Cc").toList).flatMap(Address.parseList).map(a => a.address -> a.nameDecoded))
            _ <- queue.log("submit", "sent", ip, envFrom, rcpts, signed.length, None, via)
          yield Right((delivered, queued))
      )
    yield res

  private def removeHeader(raw: Array[Byte], name: String): Array[Byte] = RawHeaders.filter(raw)(!_.name.equalsIgnoreCase(name))

  /** 配送できなかったことを差出人に知らせる（このサーバーのユーザーなら直接、外なら送信待ちに）。 */
  def bounceToSender(sender: String, original: Array[Byte], failed: List[(String, String)]): IO[Unit] =
    if sender.isEmpty then IO.unit
    else
      val pm = MailAddress(Some("Mail Delivery System"), "MAILER-DAEMON", cfg.hostname)
      val dsn = Compose.bounce(pm, sender, original, failed, cfg.hostname, Instant.now(), cfg.zone)
      blobs.put(dsn).flatMap { id =>
        resolve(sender).flatMap {
          case Some(t) => deliverToUsers(t.users, dsn, id, None, "", junk = false).void
          case None    => isLocalDomain(Address.domainOf(sender)).flatMap(l => if l then IO.unit else queue.enqueue(None, "", List(sender), id).void)
        }
      }

object Delivery:
  /** DMARC の集計レポートを受け取るアドレスのローカル部（dmarc-reports@<ドメイン>）。 */
  val DmarcReportsLocal = "dmarc-reports"
  /** これより多くの Received があるメールはループとみなす（RFC 5321 6.3 の目安は 100）。 */
  val MaxHops = 50

/** DKIM の署名（From のドメインの鍵で）。 */
final class Signer(domains: DomainStore, box: skereru.auth.SecretBox, dns: Option[Dns] = None):

  private def key(domain: String): IO[Option[(Domain, java.security.PrivateKey)]] =
    domains.domainByName(domain).map(_.flatMap { dom =>
      box.decrypt(dom.dkimPrivate).flatMap(k => scala.util.Try(Dkim.privateKey(java.util.Base64.getDecoder.decode(k))).toOption).map(dom -> _)
    })

  /** 転送するメールに ARC の封印を付ける（転送先で、このサーバーが受け取ったときの認証の結果を引き継いでもらう。
    * 転送すると SPF・DKIM が通らなくなりやすく、DMARC で拒否されるのを防ぐ）。domain は転送するアドレスのドメイン。
    * authHeader は受信時に付けた Authentication-Results のヘッダー全体。 */
  def arcSeal(raw: Array[Byte], domain: String, authHeader: Option[String], hostname: String): IO[Array[Byte]] =
    key(domain).flatMap {
      case None => IO.pure(raw)
      case Some((dom, pk)) =>
        IO.blocking {
          val chain = dns.fold(ArcResult("none", 0, ""))(d => new Arc(d).verify(raw, Instant.now()))
          // "Authentication-Results: host;\r\n\tspf=…" から、host; の後ろを1行にして渡す
          val results = authHeader.map(h => h.drop(h.indexOf(';') + 1).replaceAll("\r?\n[ \t]*", " ").trim).filter(_.nonEmpty).getOrElse("none")
          val seal = Arc.seal(raw, dom.name, dom.dkimSelector, pk, hostname, results, chain, Instant.now())
          seal.getBytes(StandardCharsets.ISO_8859_1) ++ raw
        }
    }

  def sign(raw: Array[Byte]): IO[Array[Byte]] =
    val root = Mime.parse(raw)
    root.header("From").flatMap(Address.parseOne).map(_.domain) match
      case None => IO.pure(raw)
      case Some(d) =>
        domains.domainByName(d).map {
          case None => raw
          case Some(dom) =>
            box.decrypt(dom.dkimPrivate).flatMap(k => scala.util.Try(java.util.Base64.getDecoder.decode(k)).toOption) match
              case None => raw
              case Some(pk) =>
                Dkim.sign(raw, dom.name, dom.dkimSelector, Dkim.privateKey(pk), Instant.now()).getBytes(StandardCharsets.ISO_8859_1) ++ raw
        }
