package skereru.api

import cats.effect.*
import org.http4s.*
import org.typelevel.ci.CIString
import sbirds.auth.Crypto
import sbirds.http.{HttpParts, SessionCookie}
import skereru.auth.ApiKeys
import skereru.db.*

import java.time.Instant

/** 認証: Cookie のセッション、または `Authorization: Bearer <APIキー>`。 */
final class SessionAuth(users: UserStore, cfg: ApiConfig) {

  private val cookie = new SessionCookie(cfg.cookieName, cfg.cookieSecure, cfg.sessionTtl)

  private def isAdmin(u: User, gs: List[String]) = u.isAdmin || gs.exists(cfg.adminGroups.contains)

  /**
   * APIキーでは、グループによる管理者の権限は、最近ログインして（グループを同期して）いるときだけ認める。
   * IdP で管理者のグループから外しても、ログインしない限りこちらのグループは古いままなので。
   */
  private def isAdminForKey(u: User, gs: List[String], now: Instant) =
    u.isAdmin ||
      (gs.exists(cfg.adminGroups.contains) &&
        u.lastLoginAt.exists(_.isAfter(now.minusSeconds(cfg.adminGroupFreshness.toSeconds))))

  def fromRequest(req: Request[IO]): IO[Option[AuthSession]] =
    bearer(req) match {
      case Some(token) =>
        val now = Instant.now()
        users.findApiKey(Crypto.sha256(token), now).map(_.map {
          (u, gs, kid, scopes) =>
            {
              val mcp = req.pathInfo.renderString.startsWith("/mcp") ||
                req.headers.get(CIString("X-Skereru-Mcp")).isDefined
              AuthSession(u, gs, isAdminForKey(u, gs, now), AuthVia.ApiKey(kid, scopes.toSet, mcp))
            }
        })
      case None =>
        cookie.hash(req) match {
          case None => IO.pure(None)
          case Some(hash) =>
            users.findSession(hash, Instant.now()).map(_.map((u, gs) => AuthSession(u, gs, isAdmin(u, gs))))
        }
    }

  private def bearer(req: Request[IO]): Option[String] = HttpParts.bearer(req).filter(ApiKeys.looksLike)

  /** セッションを発行する。管理者のセッションは短くする（IdP での権限の取り消しを早く反映するため）。 */
  def issue(user: User, req: Request[IO], idToken: Option[String] = None): IO[ResponseCookie] = {
    val token = Crypto.randomToken()
    users.groupsIO(user.id).flatMap { gs =>
      val ttl = if isAdmin(user, gs) then cfg.adminSessionTtl else cfg.sessionTtl
      users.createSession(
        Crypto.sha256(token),
        user.id,
        Instant.now().plusSeconds(ttl.toSeconds),
        Some(ReqInfo.clientIp(req)),
        req.headers.get(CIString("User-Agent")).map(_.head.value),
        idToken
      ).as(cookie.cookie(token, ttl.toSeconds))
    }
  }

  /** sbirds のアプリの表示名（ログインしたときの ID トークンにある. API キーや開発用のログインでは空）. */
  def appNames(req: Request[IO]): IO[Map[String, String]] = cookie.appNames(req)(users.sessionIdToken)

  /** セッションを消す. IdP からもログアウトするための id_token（OIDC でログインしていたとき）と、Cookie を消す指示を返す. */
  def revoke(req: Request[IO]): IO[(Option[String], ResponseCookie)] = cookie.revoke(req)(users.deleteSession)
}
