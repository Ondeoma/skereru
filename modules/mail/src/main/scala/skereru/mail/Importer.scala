package skereru.mail

import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.*
import skereru.auth.SecretBox
import skereru.core.*
import skereru.db.*
import skereru.shared.*

import java.io.ByteArrayOutputStream
import java.net.{InetAddress, InetSocketAddress, Socket, SocketAddress}
import java.time.Instant
import java.util.{Properties, UUID}
import scala.concurrent.duration.*

/** 1通の取り込みの結果。 */
enum ImportOutcome:
  case Imported, Duplicate, TooLarge, OverQuota

/** IMAP の取り込みの接続先（暗号化して import_job に置く）。 */
final case class ImapSource(host: String, port: Int, user: String, password: String) derives io.circe.Encoder.AsObject, io.circe.Decoder

/** ほかのサービス（Gmail など）からのメールの取り込み。
  * 同じ本体（内容の HMAC が同じ）のメッセージがすでにあれば取り込まないので、途中で止まっても最初からやり直せばよい。 */
final class Importer(mail: MailStore, store: ImportStore, blobs: skereru.storage.Blobs, box: SecretBox, dns: JndiDns, cfg: MailConfig,
    log: String => Unit, notify: UUID => IO[Unit]):

  // ---------------------------------------------------------------- 1通の取り込み（IMAP・ファイルで共通）

  /** 取り込み先のメールボックス（フォルダはなければ作る）。1回の取り込みの間、名前ごとに覚えておく。 */
  final class Boxes(userId: UUID):
    private val cache = scala.collection.mutable.Map.empty[ImportTarget, UUID]
    def apply(t: ImportTarget): IO[UUID] = cache.get(t) match
      case Some(id) => IO.pure(id)
      case None =>
        val found: IO[Mailbox] = t match
          case ImportTarget.Special(u) =>
            mail.special(userId, u).flatMap(_.fold(mail.ensureDefaults(userId) *> mail.special(userId, u).map(_.get))(IO.pure))
          case ImportTarget.Folder(n) =>
            mail.mailboxByName(userId, n).flatMap {
              case Some(b) => IO.pure(b)
              case None => mail.createMailbox(userId, n).flatMap {
                case Right(b) => IO.pure(b)
                // 同時に作られた・上限を超えたなどは、「取り込み」にまとめる
                case Left(_) if n != MailImport.Fallback => this(ImportTarget.Folder(MailImport.Fallback)).flatMap(id => mail.mailbox(id).map(_.get))
                case Left(e) => IO.raiseError(new IllegalStateException(e.message))
              }
            }
        found.map { b => cache(t) = b.id; b.id }

  /** 1通を取り込む。targets のそれぞれに入れる（本体は1つだけ保存される）。 */
  def importOne(userId: UUID, raw: Array[Byte], boxes: Boxes, p: Placement, date: Option[Instant], onlyIfNew: Boolean = false): IO[ImportOutcome] =
    if raw.length > cfg.maxMessageBytes then IO.pure(ImportOutcome.TooLarge)
    else if p.targets.isEmpty then IO.pure(ImportOutcome.Duplicate)
    else
      mail.quota(userId, cfg.defaultQuotaBytes).flatMap { (quota, used) =>
        if used + raw.length > quota then IO.pure(ImportOutcome.OverQuota)
        else
          blobs.put(raw).flatMap { blobId =>
            // onlyIfNew: Gmail の「すべてのメール」は、ほかのフォルダで取り込んだものを除く（ユーザーのどこにもないものだけ）
            (if onlyIfNew then store.exists(userId, None, blobId) else IO.pure(false)).flatMap {
              case true => IO.pure(ImportOutcome.Duplicate)
              case false =>
                val root = Mime.parse(raw)
                val internal = date.orElse(root.header("Date").flatMap(Dates.parseRfc5322)).getOrElse(Instant.now())
                val flags = (if p.seen then List(Flag.Seen) else Nil) ++ (if p.flagged then List(Flag.Flagged) else Nil)
                p.targets.traverse { t =>
                  boxes(t).flatMap { boxId =>
                    store.exists(userId, Some(boxId), blobId).flatMap {
                      case true  => IO.pure(false)
                      case false => mail.insert(userId, boxId, Indexer.index(raw, blobId, internal, flags, None, None, Some(root))).as(true)
                    }
                  }
                }.map(r => if r.contains(true) then ImportOutcome.Imported else ImportOutcome.Duplicate)
            }
          }
      }

  /** ファイル（mbox・.eml）の1通の置き場所。Google Takeout の mbox は X-Gmail-Labels のとおりに、それ以外は folder に入れる。 */
  def filePlacement(raw: Array[Byte], folder: String): Placement =
    // ヘッダーだけを読む
    val headers = Mime.parseHeaders(Mime.latin1(raw, 0, Mime.headerEnd(raw, 0, raw.length)))
    headers.find(_.name.equalsIgnoreCase("X-Gmail-Labels")).map(_.raw) match
      case Some(l) => MailImport.gmailPlacement(MailImport.gmailLabels(l.replaceAll("\r?\n[ \t]", " ")))
      case None    => Placement(List(ImportTarget.Folder(MailImport.folderName(folder, '/'))), seen = true, flagged = false)

  /** ファイルの取り込みで、画面が送った何通か（[4バイトの長さ][本体]の並び）を取り込む。
    * tooLarge: 画面が大きすぎて送らなかった数（失敗に数える）。改行は CRLF にそろえる（IMAP で取り込んだものと同じ本体になる）。 */
  def importBatch(userId: UUID, jobId: UUID, framed: Array[Byte], folder: String, tooLarge: Int = 0): IO[Either[String, ImportBatchDto]] =
    store.find(userId, jobId).flatMap {
      case None => IO.pure(Left("取り込みが見つかりません"))
      case Some(j) if j.kind != "file" || j.status != "running" => IO.pure(Left("この取り込みは終わっています"))
      case Some(_) =>
        Importer.unframe(framed) match
          case None => IO.pure(Left("送られたデータの形が正しくありません"))
          case Some(msgs) =>
            val boxes = new Boxes(userId)
            // ラベルのヘッダーは置き場所に使ったら外す（書き出したものを戻したときや、IMAP で取り込んだものと同じ本体になり、重複として扱える）
            def strip(raw: Array[Byte]) = RawHeaders.filter(raw)(h => !h.name.equalsIgnoreCase("X-Gmail-Labels") && !h.name.equalsIgnoreCase("X-GM-THRID"))
            msgs.map(Compose.crlf).traverse(raw => importOne(userId, strip(raw), boxes, filePlacement(raw, folder), None).handleError(_ => ImportOutcome.TooLarge)).flatMap { rs =>
              val dto = ImportBatchDto(rs.count(_ == ImportOutcome.Imported), rs.count(_ == ImportOutcome.Duplicate),
                rs.count(o => o == ImportOutcome.TooLarge || o == ImportOutcome.OverQuota) + math.max(0, math.min(tooLarge, 10_000)))
              store.progress(jobId, dto.imported, dto.skipped, dto.failed) *> notify(userId).whenA(dto.imported > 0).as(Right(dto))
            }
    }

  // ---------------------------------------------------------------- IMAP

  /** IMAP の取り込みを始める（ログインできるか先に確かめる。接続情報は暗号化して置き、終わったら消す）。 */
  def startImap(userId: UUID, src: ImapSource): IO[Either[String, ImportJobDto]] =
    val host = src.host.trim.toLowerCase
    if host.isEmpty || src.user.trim.isEmpty || src.password.isEmpty then IO.pure(Left("サーバー・ユーザー名・パスワードを入力してください"))
    else if !(Address.validDomain(host) || host.headOption.exists(_.isDigit)) || host.length > 253 then IO.pure(Left("サーバーの名前が正しくありません"))
    else if src.port < 1 || src.port > 65535 then IO.pure(Left("ポートが正しくありません"))
    else
      val s = src.copy(host = host, user = src.user.trim)
      IO.blocking(withStore(s)(_ => ())).attempt.flatMap {
        case Left(e) => IO.pure(Left(s"ログインできませんでした: ${Importer.reason(e)}"))
        case Right(_) =>
          store.create(userId, "imap", s"${s.user} (${s.host})", Some(box.encrypt(s.asJson.noSpaces)), "queued").map(_.left.map(_.message))
      }

  /** 待っている IMAP の取り込みを1つ引き受けて、最後まで進める（定期処理から呼ぶ）。 */
  def runNext(node: String): IO[Unit] =
    store.claimImap(node).flatMap(_.traverse_ { job =>
      (IO(box.decrypt(job.secret).toRight(new IllegalStateException("接続情報を読めません")).flatMap(decode[ImapSource](_)).fold(throw _, identity)).flatMap(src => runImap(job, src)) *>
        store.finish(job.id, "done", None)).handleErrorWith { e =>
        IO(log(s"[import] ${job.id}: $e")) *> store.finish(job.id, "failed", Some(Importer.reason(e)))
      } *> notify(job.userId)
    })

  private def runImap(job: ImapJob, src: ImapSource): IO[Unit] = Dispatcher.sequential[IO].use(d => IO.blocking(withStore(src) { st =>
    import jakarta.mail.*
    val boxes = new Boxes(job.userId)
    val b = new Bridge(d)
    def run[A](io: IO[A]): A = b.run(io)
    def cancelled = run(store.status(job.id)).contains("cancelled")
    val folders = st.getDefaultFolder.list("*").toList
    val plans = folders.map { f =>
      val attrs = f match
        case i: org.eclipse.angus.mail.imap.IMAPFolder => i.getAttributes.toSet
        case _ => Set.empty[String]
      f -> MailImport.imapFolder(f.getFullName, f.getSeparator, attrs)
    }
    // 「すべてのメール」は最後に（ほかのフォルダで取り込んでいないもの＝アーカイブだけを取り込む）
    val ordered = plans.filter(_._2 != ImapFolderPlan.AllMail) ++ plans.filter(_._2 == ImapFolderPlan.AllMail)
    ordered.foreach { (f, plan) =>
      val (target, onlyIfNew) = plan match
        case ImapFolderPlan.Into(t) => (Some(t), false)
        case ImapFolderPlan.AllMail => (Some(ImportTarget.Special(SpecialUse.Archive)), true)
        case ImapFolderPlan.Skip    => (None, false)
      target.foreach { t =>
        if (f.getType & Folder.HOLDS_MESSAGES) != 0 && !cancelled then
          f.open(Folder.READ_ONLY)
          try
            run(store.progress(job.id, 0, 0, 0, Some(f.getFullName)))
            val total = f.getMessageCount
            var i = 1
            while i <= total && !cancelled do
              val batch = f.getMessages(i, math.min(total, i + 49))
              val fp = new FetchProfile()
              fp.add(FetchProfile.Item.FLAGS); fp.add(FetchProfile.Item.SIZE); fp.add(UIDFolder.FetchProfileItem.UID)
              f.fetch(batch, fp)
              var (ok, dup, bad) = (0, 0, 0)
              batch.foreach { m =>
                val outcome =
                  if m.getSize > cfg.maxMessageBytes then ImportOutcome.TooLarge
                  else
                    val out = new ByteArrayOutputStream(math.max(1024, m.getSize))
                    m.writeTo(out)
                    val p = Placement(List(t), m.isSet(Flags.Flag.SEEN), m.isSet(Flags.Flag.FLAGGED))
                    run(importOne(job.userId, out.toByteArray, boxes, p, Option(m.getReceivedDate).map(_.toInstant), onlyIfNew))
                outcome match
                  case ImportOutcome.Imported  => ok += 1
                  case ImportOutcome.Duplicate => dup += 1
                  case _                       => bad += 1
              }
              run(store.progress(job.id, ok, dup, bad) *> notify(job.userId).whenA(ok > 0))
              i += 50
          finally f.close(false)
      }
    }
  }))

  /** IMAP につないで f を呼ぶ。接続先は確かめた IP に固定する（名前を引き直させない＝DNS リバインディングの対策）。 */
  private def withStore[A](src: ImapSource)(f: jakarta.mail.Store => A): A =
    val ip = NetPolicy.resolve(dns, src.host, cfg.outboundAllowPrivate).fold(e => throw new IllegalArgumentException(e), identity)
    // 143 は STARTTLS（必須）、それ以外は最初から TLS（993 など）
    val proto = if src.port == 143 then "imap" else "imaps"
    val props = new Properties()
    props.put(s"mail.$proto.socketFactory", new PinnedSocketFactory(ip))
    props.put(s"mail.$proto.socketFactory.fallback", "false")
    props.put(s"mail.$proto.connectiontimeout", "15000")
    props.put(s"mail.$proto.timeout", "60000")
    props.put(s"mail.$proto.writetimeout", "60000")
    props.put(s"mail.$proto.peek", "true")            // 取り込み元のメールを既読にしない
    props.put(s"mail.$proto.partialfetch", "false")
    props.put(s"mail.$proto.starttls.enable", "true")
    props.put(s"mail.$proto.starttls.required", "true") // 暗号化しない接続ではパスワードを送らない
    // 試験環境（内部のアドレスに送れる設定）では、自己署名の証明書を受け付ける
    if cfg.outboundAllowPrivate then { props.put(s"mail.$proto.ssl.trust", "*"); props.put(s"mail.$proto.ssl.checkserveridentity", "false") }
    else props.put(s"mail.$proto.ssl.checkserveridentity", "true")
    val st = jakarta.mail.Session.getInstance(props).getStore(proto)
    st.connect(src.host, src.port, src.user, src.password)
    try f(st) finally scala.util.Try(st.close())

  /** 待ち（queued）の IMAP の取り込みを、どれかのノードが引き受けるまで定期的に確かめる。 */
  def loop(node: String): IO[Nothing] =
    Periodic.run(20.seconds, 10.seconds)(e => s"[import] $e")(runNext(node) *> store.expireStale())

/** 決まった IP へつなぐソケット（connect に渡された名前の IP は使わない）。 */
final class PinnedSocketFactory(ip: InetAddress) extends javax.net.SocketFactory:
  private def pinned(): Socket = new Socket:
    override def connect(ep: SocketAddress, timeout: Int): Unit = ep match
      case a: InetSocketAddress => super.connect(new InetSocketAddress(ip, a.getPort), timeout)
      case _                    => throw new java.io.IOException("unsupported address")
    override def connect(ep: SocketAddress): Unit = connect(ep, 0)
  override def createSocket(): Socket = pinned()
  override def createSocket(host: String, port: Int): Socket = { val s = pinned(); s.connect(new InetSocketAddress(ip, port), 15000); s }
  override def createSocket(host: String, port: Int, la: InetAddress, lp: Int): Socket = createSocket(host, port)
  override def createSocket(addr: InetAddress, port: Int): Socket = createSocket("", port)
  override def createSocket(addr: InetAddress, port: Int, la: InetAddress, lp: Int): Socket = createSocket("", port)

object Importer:
  /** [4バイトの長さ（ビッグエンディアン）][本体] の並びを分ける。形が正しくなければ None。 */
  def unframe(b: Array[Byte]): Option[List[Array[Byte]]] =
    val out = scala.collection.mutable.ListBuffer.empty[Array[Byte]]
    var i = 0
    var ok = true
    while ok && i < b.length do
      if i + 4 > b.length then ok = false
      else
        val n = ((b(i) & 0xff) << 24) | ((b(i + 1) & 0xff) << 16) | ((b(i + 2) & 0xff) << 8) | (b(i + 3) & 0xff)
        if n < 0 || i + 4 + n > b.length || out.size >= 10_000 then ok = false
        else { out += java.util.Arrays.copyOfRange(b, i + 4, i + 4 + n); i += 4 + n }
    Option.when(ok)(out.toList)

  /** 画面に見せる失敗の理由（パスワードなどを含めない）。 */
  def reason(e: Throwable): String = e match
    case _: jakarta.mail.AuthenticationFailedException => "ユーザー名かパスワードが違います（Gmail はアプリ パスワードが必要です）"
    case e: IllegalArgumentException                   => Option(e.getMessage).getOrElse("接続先が正しくありません")
    case _: java.net.UnknownHostException              => "サーバーが見つかりません"
    case e: jakarta.mail.MessagingException if Option(e.getNextException).exists(_.isInstanceOf[javax.net.ssl.SSLException]) =>
      "TLS で接続できませんでした（証明書を確かめてください）"
    case e: jakarta.mail.MessagingException if Option(e.getNextException).exists(_.isInstanceOf[java.io.IOException]) =>
      "サーバーにつながりませんでした"
    case e => Option(e.getMessage).map(_.replaceAll("[\r\n]+", " ").take(300)).getOrElse(e.getClass.getSimpleName)
