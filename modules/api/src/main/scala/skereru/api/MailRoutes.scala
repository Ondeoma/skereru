package skereru.api

import cats.effect.*
import fs2.Stream
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.`Content-Type`
import skereru.db.*
import skereru.shared.*

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*

/** メールボックス・メッセージ・送信・下書き・添付ファイル・変更の通知（SSE）。 */
final class MailRoutes(svc: MailService, mail: MailStore, schedules: ScheduleStore, val sessions: SessionAuth, cfg: ApiConfig, changes: ChangeHub)
    extends RouteSupport:

  private val validName = MailText.validMailboxName

  /** ダウンロードのヘッダー（RFC 6266。日本語のファイル名は filename* で）。 */
  private def disposition(kind: String, filename: String): (String, String) =
    val ascii = filename.map(c => if c >= 0x20 && c < 0x7f && c != '"' && c != '\\' then c else '_')
    val enc = java.net.URLEncoder.encode(filename, StandardCharsets.UTF_8).replace("+", "%20")
    "Content-Disposition" -> s"""$kind; filename="$ascii"; filename*=UTF-8''$enc"""

  private val safeInline = Set("image/png", "image/jpeg", "image/gif", "image/webp")

  private def bytes(data: Array[Byte], ct: String, headers: (String, String)*): IO[Response[IO]] =
    val mt = MediaType.parse(ct).getOrElse(MediaType.application.`octet-stream`)
    IO.pure(Response[IO](Status.Ok).withEntity(data)(using EntityEncoder.byteArrayEncoder).withContentType(`Content-Type`(mt))
      .putHeaders((List("X-Content-Type-Options" -> "nosniff", "Content-Security-Policy" -> "sandbox; default-src 'none'",
        "Cache-Control" -> "private, max-age=3600") ++ headers).map(h => Header.ToRaw.keyValuesToRaw(h))*))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ---------------------------------------------------------------- メールボックス

    case req @ GET -> Root / "api" / "mailboxes" => authed(req)(s => svc.mailboxes(s.user.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "mailboxes" =>
      authed(req)(s => body[CreateMailboxReq](req)(r =>
        val n = r.name.trim
        if !validName(n) then err(Status.BadRequest, "メールボックスの名前が正しくありません")
        else mail.createMailbox(s.user.id, n).flatMap {
          case Left(e) => fail(Fail.from(e))
          case Right(b) => changes.publish(s.user.id) *> Created(MailboxDto(b.id, b.name, None, 0, 0, true))
        }))

    case req @ PATCH -> Root / "api" / "mailboxes" / UuidVar(id) =>
      authed(req)(s => body[RenameMailboxReq](req)(r =>
        mail.mailbox(id).flatMap {
          case Some(b) if b.userId == s.user.id =>
            if !validName(r.name.trim) then err(Status.BadRequest, "メールボックスの名前が正しくありません")
            else respond(mail.renameMailbox(s.user.id, b.name, r.name.trim).orFail)(_ => changes.publish(s.user.id) *> NoContent())
          case _ => err(Status.NotFound, "メールボックスが見つかりません")
        }))

    case req @ DELETE -> Root / "api" / "mailboxes" / UuidVar(id) =>
      authed(req)(s => mail.mailbox(id).flatMap {
        case Some(b) if b.userId == s.user.id => respond(mail.deleteMailbox(s.user.id, b.name).orFail)(_ => changes.publish(s.user.id) *> NoContent())
        case _ => err(Status.NotFound, "メールボックスが見つかりません")
      })

    case req @ POST -> Root / "api" / "mailboxes" / UuidVar(id) / "empty" =>
      authed(req)(s => respond(svc.emptyMailbox(s.user.id, id))(n => Ok(UpdatedDto(n))))

    // ---------------------------------------------------------------- メッセージ

    case req @ GET -> Root / "api" / "messages" =>
      authed(req)(s => okJson(svc.list(s, param(req, "mailboxId").flatMap(x => scala.util.Try(java.util.UUID.fromString(x)).toOption),
        param(req, "q").getOrElse(""), intParam(req, "offset", 0, 1_000_000), math.max(1, intParam(req, "limit", 50, 200)))))

    case req @ GET -> Root / "api" / "messages" / UuidVar(id) => authed(req)(s => okJson(svc.detail(s.user.id, id)))

    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "thread" =>
      authed(req)(s => okJson(svc.thread(s.user.id, id, markRead = param(req, "markRead").contains("true") && s.can(ApiScope.Write))))

    // 無害化した HTML の本文（画面では sandbox の iframe で表示する。スクリプトは動かない）
    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "html" =>
      authed(req)(s => respond(svc.html(s.user.id, id, images = param(req, "images").contains("1"))) {
        case None => err(Status.NotFound, "HTML の本文はありません")
        case Some((h, _, allowed)) =>
          // 外部の画像を止めているときは、CSP でも読み込ませない（CSS のすき間からの開封の追跡も防ぐ）
          val img = if allowed then "img-src 'self' data: https:" else "img-src 'self' data:"
          val page = "<!doctype html><html><head><meta charset=\"utf-8\"><base target=\"_blank\">" +
            "<style>body{margin:0;padding:8px;font-family:system-ui,sans-serif;font-size:14px;line-height:1.5;color:#1e293b;background:#fff;word-wrap:break-word;overflow-wrap:anywhere}" +
            "img{max-width:100%;height:auto}table{max-width:100%}pre{white-space:pre-wrap}</style></head><body>" + h + "</body></html>"
          IO.pure(Response[IO](Status.Ok).withEntity(page)(using EntityEncoder.stringEncoder).withContentType(`Content-Type`(MediaType.text.html, Charset.`UTF-8`)).putHeaders(
            "Content-Security-Policy" -> s"sandbox allow-same-origin allow-popups allow-popups-to-escape-sandbox; default-src 'none'; $img; style-src 'unsafe-inline'; font-src data:; base-uri 'none'; form-action 'none'; frame-ancestors 'self'",
            "X-Content-Type-Options" -> "nosniff", "Referrer-Policy" -> "no-referrer", "Cache-Control" -> "private, no-store"))
      })

    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "raw" =>
      authed(req)(s => respond(svc.raw(s.user.id, id))((m, raw) =>
        bytes(raw, "message/rfc822", disposition("attachment", (if m.subject.isEmpty then "message" else m.subject.take(80)) + ".eml"))))

    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "parts" / path =>
      authed(req)(s => respond(svc.part(s.user.id, id, path)) { (name, ct, data) =>
        val inline = safeInline.contains(ct) && !param(req, "download").contains("1")
        bytes(data, if inline then ct else "application/octet-stream", disposition(if inline then "inline" else "attachment", name))
      })

    case req @ POST -> Root / "api" / "messages" / "update" =>
      authed(req)(s => body[UpdateMessagesReq](req)(r => okJson(svc.update(s.user.id, r).map(_.map(UpdatedDto(_))))))

    // ---------------------------------------------------------------- 書く

    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "compose" =>
      authed(req)(s => okJson(svc.template(s, id, param(req, "mode").getOrElse("reply"))))

    case req @ GET -> Root / "api" / "drafts" / UuidVar(id) => authed(req)(s => okJson(svc.draft(s, id)))

    case req @ POST -> Root / "api" / "uploads" =>
      authed(req)(s => withUpload(req, cfg.maxUploadBytes)((name, ct, data) => respond(svc.upload(s.user.id, name, ct, data))(Created(_))))

    case req @ POST -> Root / "api" / "send" =>
      authed(req, ApiScope.Send)(s => body[ComposeReq](req)(r =>
        respond(svc.send(s, r, ReqInfo.clientIp(req)))(sent => Created(sent))))

    // 予約送信・取り消しの猶予つきの送信（画面はこちらを使う）
    case req @ POST -> Root / "api" / "schedule" =>
      authed(req, ApiScope.Send)(s => body[ScheduleReq](req)(r =>
        respond(svc.schedule(s, r.req, r.sendAt, ReqInfo.clientIp(req)))(d => Created(d))))

    case req @ GET -> Root / "api" / "scheduled" => authed(req)(s => schedules.list(s.user.id).flatMap(Ok(_)))

    // 取り消す（作成画面に戻すための内容を返す）
    case req @ POST -> Root / "api" / "scheduled" / UuidVar(id) / "cancel" =>
      authed(req, ApiScope.Send)(s => schedules.cancel(s.user.id, id).flatMap {
        case Some(r) =>
          mail.uploads(s.user.id, r.uploadIds).flatMap(us => Ok(ScheduledCancelDto(r, us.map(u => UploadDto(u.id, u.filename, u.contentType, u.size)))))
        case None    => err(Status.Conflict, "送信済みか、すでに取り消したため、取り消せません")
      })

    case req @ POST -> Root / "api" / "messages" / "snooze" =>
      authed(req)(s => body[SnoozeReq](req)(r => okJson(svc.snooze(s.user.id, r.ids, r.until).map(_.map(UpdatedDto(_))))))

    case req @ POST -> Root / "api" / "drafts" =>
      authed(req)(s => body[ComposeReq](req)(r => respond(svc.saveDraft(s, r))(d => Created(d))))

    // ---------------------------------------------------------------- 変更の通知（Server-Sent Events）

    case req @ GET -> Root / "api" / "stream" =>
      authed(req)(s => changes.events(s.user.id).flatMap {
        case None => err(Status.TooManyRequests, "同時に開いている画面が多すぎます")
        case Some(events) => IO.pure(Response[IO](Status.Ok)
          .withContentType(`Content-Type`(MediaType.`text/event-stream`))
          .putHeaders("Cache-Control" -> "no-cache", "X-Accel-Buffering" -> "no")
          .withBodyStream(events.through(fs2.text.utf8.encode)))
      })
  }

/** 変更の通知。同じノードの配送はすぐに、ほかのノードの変更は数秒ごとの確認で知らせる。 */
final class ChangeHub(mail: MailStore, topic: fs2.concurrent.Topic[IO, java.util.UUID]):
  def publish(userId: java.util.UUID): IO[Unit] = topic.publish1(userId).void

  /** 1人が同時に開ける通知の数（つなぐたびに数秒ごとの DB の問い合わせが増えるので、上限を付ける）。 */
  private val MaxPerUser = 20
  private val open = new java.util.concurrent.ConcurrentHashMap[java.util.UUID, java.util.concurrent.atomic.AtomicInteger]()

  def events(userId: java.util.UUID): IO[Option[Stream[IO, String]]] = IO {
    val c = open.computeIfAbsent(userId, _ => new java.util.concurrent.atomic.AtomicInteger(0))
    if c.incrementAndGet() > MaxPerUser then { c.decrementAndGet(); None }
    else Some(stream(userId).onFinalize(IO { if c.decrementAndGet() <= 0 then open.remove(userId, c) }))
  }

  private def stream(userId: java.util.UUID): Stream[IO, String] =
    val local = topic.subscribe(64).filter(_ == userId).as(())
    val ticks = Stream.awakeEvery[IO](4.seconds).as(())
    val tokens = (Stream.emit(()) ++ local.merge(ticks)).evalMap(_ => mail.changeToken(userId)).changes.drop(1).map(t => s"event: change\ndata: $t\n\n")
    // 切断に気付くのは書き込みに失敗したときなので、短めの間隔で送る（閉じた接続の数をすぐに戻す）
    val keepAlive = Stream.awakeEvery[IO](10.seconds).as(": ping\n\n")
    Stream.emit("retry: 5000\n\n") ++ tokens.mergeHaltBoth(keepAlive)
