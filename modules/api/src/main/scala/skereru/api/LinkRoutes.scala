package skereru.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.links.LinkClient
import skereru.core.ErrorCode
import skereru.shared.*

import java.util.UUID

/**
 * 兄弟のサービス（stui・skawau）との OAuth 連携（クライアントの側. 認可コード + PKCE）の手続き.
 * 相手の OAUTH_CLIENTS に skereru を登録する（redirectUri は {PUBLIC_URL}/api/integrations/{サービス}/callback）.
 * 連携の始まり（connect）と戻り（callback）は画面からの移動（GET）なので、Cookie のセッションだけで受ける.
 * links は設定されたサービス（設定されていないサービスは、状態だけ「設定なし」で返す）.
 * lastError は、そのサービスの裏の処理の最後の失敗（skawau への自動の保存など）.
 */
final class LinkRoutes(links: Map[String, LinkClient],
                       publicUrl: String,
                       val sessions: SessionAuth,
                       lastError: (String, UUID) => IO[Option[String]] = (_, _) => IO.pure(None))
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "integrations" / Known(p) => getIntegration(req, p)
    case req @ GET -> Root / "api" / "integrations" / Known(p) / "connect" => getConnect(req, p)
    case req @ GET -> Root / "api" / "integrations" / Known(p) / "callback" => getCallback(req, p)
    case req @ DELETE -> Root / "api" / "integrations" / Known(p) / "connection" => deleteConnection(req, p)
  }

  private def getIntegration(req: Request[IO], p: String): IO[Response[IO]] =
    authed(req)(s =>
      (links.get(p).traverse(_.connected(s.user.id)), sessions.appNames(req), lastError(p, s.user.id)).flatMapN(
        (on, names, last) =>
          Ok(LinkStatusDto(
            links.contains(p),
            on.contains(true),
            links.get(p).map(_.cfg.url),
            names.getOrElse(p, p),
            last
          ))
      )
    )

  private def getConnect(req: Request[IO], p: String): IO[Response[IO]] =
    sessionOnly(req)(s => withLink(p)(c => c.authorizeUrl(s.user.id, redirectUri(p), Back).flatMap(redirect(_))))

  private def getCallback(req: Request[IO], p: String): IO[Response[IO]] =
    sessionOnly(req)(s =>
      withLink(p)(c =>
        param(req, "state") match {
          case None => err(ErrorCode.LinkStateInvalid)
          case Some(state) =>
            c.takeState(state).flatMap {
              case None => back(p, "expired")
              // 別の人が始めた連携の戻りは受けない（state を盗まれたとき、他人のトークンを自分に結び付けさせない）
              case Some((uid, _, _)) if uid != s.user.id => back(p, "error")
              case Some((_, verifier, _)) =>
                param(req, "code") match {
                  case None => back(p, if param(req, "error").contains("access_denied") then "denied" else "error")
                  case Some(code) =>
                    c.exchange(s.user.id, code, verifier, redirectUri(p)).flatMap(ok =>
                      back(p, if ok then "connected" else "error")
                    )
                }
            }
        }
      )
    )

  private def deleteConnection(req: Request[IO], p: String): IO[Response[IO]] =
    sessionOnly(req)(s => links.get(p).traverse_(_.disconnect(s.user.id)) *> NoContent())

  /** 連携の後に戻る画面（設定の「連携」）. */
  private val Back = "/settings?tab=integrations"

  private def redirectUri(p: String): String = s"${publicUrl.stripSuffix("/")}/api/integrations/$p/callback"

  private def back(p: String, result: String): IO[Response[IO]] =
    redirect(s"$Back&integration=$p&result=$result")

  private def withLink(p: String)(f: LinkClient => IO[Response[IO]]): IO[Response[IO]] =
    links.get(p).fold(err(ErrorCode.LinkNotConfigured))(f)

  /** 連携できるサービス（このパスはこれらのサービスのときだけ受ける）. */
  private object Known {
    def unapply(p: String): Option[String] = Option(p).filter(LinkRoutes.Providers.contains)
  }
}

object LinkRoutes {
  val Providers: Set[String] = Set("stui", "skawau")
}
