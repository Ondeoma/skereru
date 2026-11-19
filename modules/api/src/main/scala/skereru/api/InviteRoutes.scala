package skereru.api

import java.util.UUID

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skereru.shared.*

/** 会議の招待（表示・返事・カレンダーへの登録）と、兄弟のサービスとの連携の設定。 */
final class InviteRoutes(svc: InviteService, val sessions: SessionAuth) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "invite" => getMessageInvite(req, id)
    case req @ POST -> Root / "api" / "messages" / UuidVar(id) / "invite" / "reply" => postMessageInviteReply(req, id)
    case req @ POST -> Root / "api" / "messages" / UuidVar(id) / "invite" / "calendar" =>
      postMessageInviteCalendar(req, id)
    case req @ GET -> Root / "api" / "me" / "integrations" => getMeIntegrations(req)
    case req @ PUT -> Root / "api" / "me" / "integrations" / "skuaka" => putMeIntegrationsSkuaka(req)
    case req @ DELETE -> Root / "api" / "me" / "integrations" / "skuaka" => deleteMeIntegrationsSkuaka(req)
  }

  private def getMessageInvite(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(s => okJson(svc.invite(s, id)))

  private def postMessageInviteReply(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req, ApiScope.Send)(s =>
      body[InviteReplyReq](req)(r =>
        respond(svc.reply(s, id, r.partstat, ReqInfo.clientIp(req)))(_ => Ok(UpdatedDto(1)))
      )
    )

  private def postMessageInviteCalendar(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req, ApiScope.Write)(s => okJson(svc.addToCalendar(s, id)))

  private def getMeIntegrations(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => svc.integrations(s.user.id).flatMap(Ok(_)))

  private def putMeIntegrationsSkuaka(req: Request[IO]): IO[Response[IO]] =
    sessionOnly(req)(s =>
      body[TokenReq](req)(r =>
        respond(svc.connect(s.user.id, r.token))(_ => svc.integrations(s.user.id).flatMap(Ok(_)))
      )
    )

  private def deleteMeIntegrationsSkuaka(req: Request[IO]): IO[Response[IO]] =
    sessionOnly(req)(s => svc.disconnect(s.user.id) *> svc.integrations(s.user.id).flatMap(Ok(_)))

}
