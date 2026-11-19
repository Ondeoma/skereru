package skereru.api

import cats.effect.*
import org.http4s.*
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import sbirds.auth.Crypto
import skereru.auth.ApiKeys
import skereru.db.*

import java.time.Instant

/** 認証: Cookie のセッション、または `Authorization: Bearer <APIキー>`。 */
final class SessionAuth(
  users: UserStore,
  cfg: ApiConfig) {

  private def isAdmin(
    u: User,
    gs: List[String]
  ) = u.isAdmin || gs.exists(cfg.adminGroups.contains)

  /**
   * APIキーでは、グループによる管理者の権限は、最近ログインして（グループを同期して）いるときだけ認める。
   * IdP で管理者のグループから外しても、ログインしない限りこちらのグループは古いままなので。
   */
  private def isAdminForKey(
    u: User,
    gs: List[String],
    now: Instant
  ) =
    u.isAdmin ||
      (gs.exists(cfg.adminGroups.contains) &&
        u.lastLoginAt.exists(_.isAfter(now.minusSeconds(cfg.adminGroupFreshness.toSeconds))))

  def fromRequest(
    req: Request[IO]
  ): IO[Option[AuthSession]] =
    bearer(req) match {
      case Some(token) =>
        val now = Instant.now()
        users.findApiKey(Crypto.sha256(token), now).map(_.map {
          (
            u,
            gs,
            kid,
            scopes
          ) =>
            {
              val mcp = req.pathInfo.renderString.startsWith("/mcp") ||
                req.headers.get(CIString("X-Skereru-Mcp")).isDefined
              AuthSession(u, gs, isAdminForKey(u, gs, now), AuthVia.ApiKey(kid, scopes.toSet, mcp))
            }
        })
      case None =>
        req.cookies.find(_.name == cfg.cookieName).map(_.content) match {
          case None => IO.pure(None)
          case Some(token) => users.findSession(Crypto.sha256(token), Instant.now()).map(_.map(
              (
                u,
                gs
              ) => AuthSession(u, gs, isAdmin(u, gs))
            ))
        }
    }

  private def bearer(
    req: Request[IO]
  ): Option[String] =
    req.headers.get[Authorization].collect { case Authorization(Credentials.Token(AuthScheme.Bearer, t)) => t }.filter(
      ApiKeys.looksLike
    )

  /** セッションを発行する。管理者のセッションは短くする（IdP での権限の取り消しを早く反映するため）。 */
  def issue(
    user: User,
    req: Request[IO],
    idToken: Option[String] = None
  ): IO[ResponseCookie] = {
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
      ).as(cookie(token, ttl.toSeconds))
    }
  }

  /** sbirds のアプリの表示名（ログインしたときの ID トークンにある. API キーや開発用のログインでは空）. */
  def appNames(
    req: Request[IO]
  ): IO[Map[String, String]] =
    req.cookies.find(_.name == cfg.cookieName) match {
      case Some(c) =>
        users.sessionIdToken(Crypto.sha256(c.content)).map(_.fold(Map.empty)(sbirds.auth.AppNames.fromIdToken))
      case None => IO.pure(Map.empty)
    }

  /** セッションを消す. IdP からもログアウトするための id_token（OIDC でログインしていたとき）と、Cookie を消す指示を返す. */
  def revoke(
    req: Request[IO]
  ): IO[(Option[String], ResponseCookie)] =
    req.cookies.find(_.name == cfg.cookieName) match {
      case Some(c) => users.deleteSession(Crypto.sha256(c.content)).map(_ -> cookie("", 0))
      case None => IO.pure(None -> cookie("", 0))
    }

  private def cookie(
    value: String,
    maxAge: Long
  ): ResponseCookie =
    ResponseCookie(
      cfg.cookieName,
      value,
      maxAge = Some(maxAge),
      path = Some("/"),
      httpOnly = true,
      secure = cfg.cookieSecure,
      sameSite = Some(SameSite.Lax)
    )
}
