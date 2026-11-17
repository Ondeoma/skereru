package skereru.api

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skereru.shared.*

/** 会議の招待（表示・返事・カレンダーへの登録）と、兄弟のサービスとの連携の設定。 */
final class InviteRoutes(svc: InviteService, val sessions: SessionAuth) extends RouteSupport:

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "messages" / UuidVar(id) / "invite" => authed(req)(s => okJson(svc.invite(s, id)))

    case req @ POST -> Root / "api" / "messages" / UuidVar(id) / "invite" / "reply" =>
      authed(req, ApiScope.Send)(s => body[InviteReplyReq](req)(r => respond(svc.reply(s, id, r.partstat, ReqInfo.clientIp(req)))(_ => Ok(UpdatedDto(1)))))

    case req @ POST -> Root / "api" / "messages" / UuidVar(id) / "invite" / "calendar" =>
      authed(req, ApiScope.Write)(s => okJson(svc.addToCalendar(s, id)))

    case req @ GET -> Root / "api" / "me" / "integrations" => authed(req)(s => svc.integrations(s.user.id).flatMap(Ok(_)))

    // ほかのサービスのトークンを預かるので、画面からだけ
    case req @ PUT -> Root / "api" / "me" / "integrations" / "skuaka" =>
      sessionOnly(req)(s => body[TokenReq](req)(r => respond(svc.connect(s.user.id, r.token))(_ => svc.integrations(s.user.id).flatMap(Ok(_)))))

    case req @ DELETE -> Root / "api" / "me" / "integrations" / "skuaka" =>
      sessionOnly(req)(s => svc.disconnect(s.user.id) *> svc.integrations(s.user.id).flatMap(Ok(_)))
  }
