package skereru.api

import java.util.UUID

import skereru.core.ErrorCode

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.`Content-Type`
import skereru.db.*
import skereru.shared.*

import java.nio.charset.StandardCharsets

/** メールボックス・メッセージ・送信・下書き・添付ファイル・変更の通知（SSE）。 */
final class MailRoutes(
  svc: MailService,
  mail: MailStore,
  schedules: ScheduleStore,
  val sessions: SessionAuth,
  cfg: ApiConfig,
  changes: ChangeHub)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ---------------------------------------------------------------- メールボックス

    case req @ GET -> Root / "api" / "mailboxes" => getMailboxes(req)
    case req @ POST -> Root / "api" / "mailboxes" => postMailboxes(req)
    case req @ PATCH -> Root / "api" / "mailboxes" / UuidVar(id) => patchMailbox(req, id)
    case req @ DELETE -> Root / "api" / "mailboxes" / UuidVar(id) => deleteMailbox(req, id)
    case req @ POST -> Root / "api" / "mailboxes" / UuidVar(id) / "empty" => postMailboxEmpty(req, id)
    case req @ GET -> Root / "api" / "messages" => getMessages(req)
    case req @ GET -> Root / "api" / "messages" / UuidVar(id) => getMessage(req, id)
    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "thread" => getMessageThread(req, id)
    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "html" => getMessageHtml(req, id)
    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "raw" => getMessageRaw(req, id)
    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "parts" / path => getMessagePart(req, id, path)
    case req @ POST -> Root / "api" / "messages" / "update" => postMessagesUpdate(req)
    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "compose" => getMessageCompose(req, id)
    case req @ GET -> Root / "api" / "drafts" / UuidVar(id) => getDraft(req, id)
    case req @ POST -> Root / "api" / "uploads" => postUploads(req)
    case req @ POST -> Root / "api" / "send" => postSend(req)
    case req @ POST -> Root / "api" / "schedule" => postSchedule(req)
    case req @ GET -> Root / "api" / "scheduled" => getScheduled(req)
    case req @ POST -> Root / "api" / "scheduled" / UuidVar(id) / "cancel" => postScheduledCancel(req, id)
    case req @ POST -> Root / "api" / "messages" / "snooze" => postMessagesSnooze(req)
    case req @ POST -> Root / "api" / "drafts" => postDrafts(req)
    case req @ GET -> Root / "api" / "stream" => getStream(req)
  }

  private def getMailboxes(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s => svc.mailboxes(s.user.id).flatMap(Ok(_)))

  private def postMailboxes(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      body[CreateMailboxReq](req)(r => {
        val n = r.name.trim
        if !validName(n) then err(ErrorCode.MailboxNameInvalid)
        else
          mail.createMailbox(s.user.id, n).flatMap {
            case Left(e) => fail(e)
            case Right(b) => changes.publish(s.user.id) *> Created(MailboxDto(b.id, b.name, None, 0, 0, true))
          }
      })
    )

  private def patchMailbox(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s =>
      body[RenameMailboxReq](req)(r =>
        mail.mailbox(id).flatMap {
          case Some(b) if b.userId == s.user.id =>
            if !validName(r.name.trim) then err(ErrorCode.MailboxNameInvalid)
            else
              respond(mail.renameMailbox(s.user.id, b.name, r.name.trim))(_ =>
                changes.publish(s.user.id) *> NoContent()
              )
          case _ => err(ErrorCode.MailboxNotFound)
        }
      )
    )

  private def deleteMailbox(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s =>
      mail.mailbox(id).flatMap {
        case Some(b) if b.userId == s.user.id =>
          respond(mail.deleteMailbox(s.user.id, b.name))(_ => changes.publish(s.user.id) *> NoContent())
        case _ => err(ErrorCode.MailboxNotFound)
      }
    )

  private def postMailboxEmpty(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s => respond(svc.emptyMailbox(s.user.id, id))(n => Ok(UpdatedDto(n))))

  private def getMessages(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      okJson(svc.list(
        s,
        param(req, "mailboxId").flatMap(x => scala.util.Try(java.util.UUID.fromString(x)).toOption),
        param(req, "q").getOrElse(""),
        intParam(req, "offset", 0, 1_000_000),
        math.max(1, intParam(req, "limit", 50, 200))
      ))
    )

  private def getMessage(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s => okJson(svc.detail(s.user.id, id)))

  private def getMessageThread(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s =>
      okJson(svc.thread(s.user.id, id, markRead = param(req, "markRead").contains("true") && s.can(ApiScope.Write)))
    )

  private def getMessageHtml(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s =>
      respond(svc.html(s.user.id, id, images = param(req, "images").contains("1"))) {
        case None => err(ErrorCode.HtmlBodyMissing)
        case Some((h, _, allowed)) =>
          // 外部の画像を止めているときは、CSP でも読み込ませない（CSS のすき間からの開封の追跡も防ぐ）
          val img = if allowed then "img-src 'self' data: https:" else "img-src 'self' data:"
          val page = "<!doctype html><html><head><meta charset=\"utf-8\"><base target=\"_blank\">" +
            "<style>body{margin:0;padding:8px;font-family:system-ui,sans-serif;font-size:14px;line-height:1.5;color:#1e293b;background:#fff;word-wrap:break-word;overflow-wrap:anywhere}" +
            "img{max-width:100%;height:auto}table{max-width:100%}pre{white-space:pre-wrap}</style></head><body>" + h +
            "</body></html>"
          IO.pure(Response[IO](Status.Ok).withEntity(page)(using EntityEncoder.stringEncoder).withContentType(
            `Content-Type`(MediaType.text.html, Charset.`UTF-8`)
          ).putHeaders(
            "Content-Security-Policy" ->
              s"sandbox allow-same-origin allow-popups allow-popups-to-escape-sandbox; default-src 'none'; $img; style-src 'unsafe-inline'; font-src data:; base-uri 'none'; form-action 'none'; frame-ancestors 'self'",
            "X-Content-Type-Options" -> "nosniff",
            "Referrer-Policy" -> "no-referrer",
            "Cache-Control" -> "private, no-store"
          ))
      }
    )

  private def getMessageRaw(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s =>
      respond(svc.raw(s.user.id, id))(
        (
          m,
          raw
        ) =>
          bytes(
            raw,
            "message/rfc822",
            disposition("attachment", (if m.subject.isEmpty then "message" else m.subject.take(80)) + ".eml")
          )
      )
    )

  private def getMessagePart(
    req: Request[IO],
    id: UUID,
    path: String
  ): IO[Response[IO]] =
    authed(req)(s =>
      respond(svc.part(s.user.id, id, path)) {
        (
          name,
          ct,
          data
        ) =>
          {
            val inline = safeInline.contains(ct) && !param(req, "download").contains("1")
            bytes(
              data,
              if inline then ct else "application/octet-stream",
              disposition(if inline then "inline" else "attachment", name)
            )
          }
      }
    )

  private def postMessagesUpdate(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s => body[UpdateMessagesReq](req)(r => okJson(svc.update(s.user.id, r).map(_.map(UpdatedDto(_))))))

  private def getMessageCompose(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s => okJson(svc.template(s, id, param(req, "mode").getOrElse("reply"))))

  private def getDraft(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s => okJson(svc.draft(s, id)))

  private def postUploads(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      withUpload(req, cfg.maxUploadBytes)(
        (
          name,
          ct,
          data
        ) => respond(svc.upload(s.user.id, name, ct, data))(Created(_))
      )
    )

  private def postSend(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req, ApiScope.Send)(s =>
      body[ComposeReq](req)(r =>
        respond(svc.send(s, r, ReqInfo.clientIp(req)))(sent => Created(sent))
      )
    )

  private def postSchedule(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req, ApiScope.Send)(s =>
      body[ScheduleReq](req)(r =>
        respond(svc.schedule(s, r.req, r.sendAt, ReqInfo.clientIp(req)))(d => Created(d))
      )
    )

  private def getScheduled(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s => schedules.list(s.user.id).flatMap(Ok(_)))

  private def postScheduledCancel(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req, ApiScope.Send)(s =>
      schedules.cancel(s.user.id, id).flatMap {
        case Some(r) =>
          mail.uploads(s.user.id, r.uploadIds).flatMap(us =>
            Ok(ScheduledCancelDto(r, us.map(u => UploadDto(u.id, u.filename, u.contentType, u.size))))
          )
        case None => err(ErrorCode.ScheduledSendNotCancellable)
      }
    )

  private def postMessagesSnooze(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      body[SnoozeReq](req)(r => okJson(svc.snooze(s.user.id, r.ids, r.until).map(_.map(UpdatedDto(_)))))
    )

  private def postDrafts(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s => body[ComposeReq](req)(r => respond(svc.saveDraft(s, r))(d => Created(d))))

  private def getStream(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      changes.events(s.user.id).flatMap {
        case None => err(ErrorCode.EventStreamLimitReached)
        case Some(events) => IO.pure(Response[IO](Status.Ok)
            .withContentType(`Content-Type`(MediaType.`text/event-stream`))
            .putHeaders("Cache-Control" -> "no-cache", "X-Accel-Buffering" -> "no")
            .withBodyStream(events.through(fs2.text.utf8.encode)))
      }
    )

  private val validName = MailText.validMailboxName

  /** ダウンロードのヘッダー（RFC 6266。日本語のファイル名は filename* で）。 */
  private def disposition(
    kind: String,
    filename: String
  ): (String, String) = {
    val ascii = filename.map(c => if c >= 0x20 && c < 0x7f && c != '"' && c != '\\' then c else '_')
    val enc = java.net.URLEncoder.encode(filename, StandardCharsets.UTF_8).replace("+", "%20")
    "Content-Disposition" -> s"""$kind; filename="$ascii"; filename*=UTF-8''$enc"""
  }

  private val safeInline = Set("image/png", "image/jpeg", "image/gif", "image/webp")

  private def bytes(
    data: Array[Byte],
    ct: String,
    headers: (String, String)*
  ): IO[Response[IO]] = {
    val mt = MediaType.parse(ct).getOrElse(MediaType.application.`octet-stream`)
    IO.pure(
      Response[IO](Status.Ok).withEntity(data)(using EntityEncoder.byteArrayEncoder).withContentType(`Content-Type`(mt))
        .putHeaders((List(
          "X-Content-Type-Options" -> "nosniff",
          "Content-Security-Policy" -> "sandbox; default-src 'none'",
          "Cache-Control" -> "private, max-age=3600"
        ) ++ headers).map(h => Header.ToRaw.keyValuesToRaw(h))*)
    )
  }

}
