package skereru.api

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.links.{LinkFailure, StuiClient}
import skereru.core.ErrorCode
import skereru.shared.*

import java.util.UUID

/**
 * 受信の通知先（stui・受信 Webhook・署名付きの Webhook）と、通知先に選べる stui のチャンネル.
 * stui との連携の手続き（connect・callback）は LinkRoutes.
 */
final class NotifyRoutes(svc: NotifyService, stui: Option[StuiClient], val sessions: SessionAuth) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "me" / "notify-targets" => getNotifyTargets(req)
    case req @ POST -> Root / "api" / "me" / "notify-targets" => postNotifyTargets(req)
    case req @ PUT -> Root / "api" / "me" / "notify-targets" / UuidVar(id) => putNotifyTarget(req, id)
    case req @ DELETE -> Root / "api" / "me" / "notify-targets" / UuidVar(id) => deleteNotifyTarget(req, id)
    case req @ POST -> Root / "api" / "me" / "notify-targets" / UuidVar(id) / "test" => postNotifyTargetTest(req, id)
    case req @ GET -> Root / "api" / "integrations" / "stui" / "channels" => getStuiChannels(req)
  }

  private def getNotifyTargets(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => svc.list(s.user.id).flatMap(Ok(_)))

  private def postNotifyTargets(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => body[NotifyTargetReq](req)(r => respond(svc.create(s.user.id, r))(Created(_))))

  private def putNotifyTarget(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(s => body[NotifyTargetReq](req)(r => okJson(svc.update(s.user.id, id, r))))

  private def deleteNotifyTarget(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(s => respond(svc.delete(s.user.id, id))(_ => NoContent()))

  private def postNotifyTargetTest(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(s => respond(svc.test(s.user.id, id, s.user.name))(_ => NoContent()))

  private def getStuiChannels(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      stui.fold(err(ErrorCode.StuiNotConfigured))(c =>
        c.channels(s.user.id).flatMap {
          case Right(cs) => Ok(cs.map(x => StuiChannelDto(x.id, x.name, x.kind)))
          case Left(LinkFailure.NotConnected) | Left(LinkFailure.Expired) => err(ErrorCode.StuiNotConnected)
          case Left(LinkFailure.Rejected(code)) => err(ErrorCode.StuiRequestFailed, s"stui が $code を返しました")
          case Left(LinkFailure.Unreachable(_)) => err(ErrorCode.StuiRequestFailed, "stui に接続できませんでした")
        }
      )
    )
}
