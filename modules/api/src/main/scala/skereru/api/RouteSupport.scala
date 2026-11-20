package skereru.api

import cats.effect.*
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.http.{Csrf, Errors, RouteHelpers}
import sbirds.shared.AppError
import skereru.core.ErrorCode
import skereru.db.*
import skereru.shared.*

/**
 * *Routes に共通の補助（エラー応答・認証・スコープ・CSRF）。本文・クエリ・パスの UUID・302 は sbirds.http.RouteHelpers。
 */
trait RouteSupport extends RouteHelpers {
  protected def sessions: SessionAuth

  protected def bodyInvalid: sbirds.shared.ErrorCode = ErrorCode.RequestBodyInvalid

  // 本文の読み取りの途中のエラー（大きすぎる本文を含む）は、これまでどおり RequestBodyInvalid（400）にする
  override protected def bodyKeepsAppError: Boolean = false

  /** 0 以上 max 以下の整数のパラメータ（offset にも使うので、下限は RouteHelpers の 1 でなく 0）。 */
  override protected def intParam(req: Request[IO], k: String, default: Int, max: Int): Int =
    param(req, k).flatMap(_.toIntOption).map(v => math.max(0, math.min(v, max))).getOrElse(default)

  /** エラーの応答（コード付きでログに出る）. detail を省くとコードの既定の文言. */
  protected def err(code: ErrorCode, detail: String | Null = null): IO[Response[IO]] =
    Errors.respond(AppError(code, Option(detail)))

  protected def fail(e: AppError): IO[Response[IO]] = Errors.respond(e)

  protected def respond[A](r: Res[A])(f: A => IO[Response[IO]]): IO[Response[IO]] = r.flatMap(_.fold(fail, f))
  protected def okJson[A: io.circe.Encoder](r: Res[A]): IO[Response[IO]] = respond(r)(a => Ok(a))

  private val safe = Set(Method.GET, Method.HEAD, Method.OPTIONS)

  /** 画面が付ける専用のヘッダー（X-Skereru）があるかを確かめる（オリジンは照合しない）。ログアウトでも使う。 */
  protected val csrf: Csrf.Policy = Csrf.Policy.headerOnly("X-Skereru", Csrf.SafeWithOptions)

  /**
   * ログインしていなければ 401。スコープが足りなければ 403。
   * Cookie で更新系の要求をするときは X-Skereru ヘッダーを必須にする（ほかのサイトからは付けられないので CSRF を防げる）。
   */
  protected def authed(req: Request[IO], scope: String = "")(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    sessions.fromRequest(req).flatMap {
      case None => err(ErrorCode.NotLoggedIn)
      case Some(s) =>
        val need = if scope.nonEmpty then scope else if safe.contains(req.method) then ApiScope.Read else ApiScope.Write
        // 操作したユーザーを、トレース（スパンの属性）とこの中のログに載せる
        sbirds.observe.Telemetry.withUser(s.user.id.toString, s.user.name) {
          if s.isSession && Csrf.foreign(req, csrf) then err(ErrorCode.CsrfHeaderMissing)
          else if !s.can(need) then
            if need == ApiScope.Admin then err(ErrorCode.AdminOnly)
            else err(ErrorCode.ApiKeyScopeMissing, s"このAPIキーには $need のスコープがありません")
          else f(s)
        }
    }

  protected def admin(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    authed(req, ApiScope.Admin)(f)

  /** Cookie のセッションのみ（APIキー・アプリパスワードの発行など）。 */
  protected def sessionOnly(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(onlySession(f))

  /** 管理者の、Cookie のセッションのみ（APIキーでエージェント専用アカウントを管理させない）。 */
  protected def adminSessionOnly(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    admin(req)(onlySession(f))

  private def onlySession(f: AuthSession => IO[Response[IO]])(s: AuthSession): IO[Response[IO]] =
    if s.isSession then f(s) else err(ErrorCode.SessionOnly)

  /** multipart/form-data の file を受け取る（上限を超えたら 413）。 */
  protected def withUpload(req: Request[IO],
                           maxBytes: Long)(f: (String, String, Array[Byte]) => IO[Response[IO]]): IO[Response[IO]] =
    req.as[org.http4s.multipart.Multipart[IO]].attempt.flatMap {
      case Left(_) => err(ErrorCode.UploadNotMultipart)
      case Right(mp) =>
        mp.parts.find(_.name.contains("file")) match {
          case None => err(ErrorCode.UploadFileMissing)
          case Some(part) =>
            val name = part.filename.map(_.trim).filter(_.nonEmpty).getOrElse("file").replaceAll(
              "[\\\\/\\r\\n\\u0000]",
              "_"
            ).take(200)
            val ct = part.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}").getOrElse(
              "application/octet-stream"
            )
            part.body.take(maxBytes + 1).compile.to(Array).flatMap { bytes =>
              if bytes.length > maxBytes then
                err(ErrorCode.UploadTooLarge, s"ファイルが大きすぎます（上限 ${math.max(1, maxBytes / 1024 / 1024)}MB）")
              else f(name, ct, bytes)
            }
        }
    }

  protected def audit(users: UserStore,
                      s: AuthSession,
                      req: Request[IO],
                      action: String,
                      target: Option[String],
                      detail: Json = Json.obj()): IO[Unit] =
    users.audit(Some(s.user.id), s.viaString, action, target, Some(ReqInfo.clientIp(req)), detail)
}
