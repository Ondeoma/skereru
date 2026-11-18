package skereru.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.{Decoder, Json}
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import skereru.auth.{ApiKeys, Crypto}
import skereru.db.*
import skereru.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

final case class ApiConfig(
    adminGroups: Set[String],
    cookieSecure: Boolean,
    publicUrl: String,
    sessionTtl: FiniteDuration = 14.days,
    /** 管理者のセッションの長さ。IdP で管理者のグループから外されたら、次のログイン（グループの同期）で権限がなくなるので短くする。 */
    adminSessionTtl: FiniteDuration = 8.hours,
    /** IdP のグループによる管理者の権限を、APIキーで使える期間（最後のログイン＝グループの同期から）。過ぎたらログインし直すまで管理の API を使えない。 */
    adminGroupFreshness: FiniteDuration = 7.days,
    devLogin: Boolean = false,
    maxUploadBytes: Long = 25L * 1024 * 1024,
    timezone: String = "Asia/Tokyo",
    version: String = "0.1.0",
    /** IdP でログインした人を、自動で作るか（メールアドレスのドメインが auto_provision のときだけアドレスも作る）。 */
    autoCreateUsers: Boolean = true,
    /** 認可を求める外部のサービス（OAUTH_CLIENTS）。 */
    oauthClients: List[OAuthClient] = Nil
):
  /** https なら __Host- の付いた Cookie（Secure・Path=/・Domain なしが強制される）。 */
  val cookieName: String = if cookieSecure then "__Host-skereru_session" else "skereru_session"
  /** ログインの途中（IdP へ行って戻るまで）の state を入れる Cookie。戻ってきたブラウザが、ログインを始めたブラウザと同じか確かめる。 */
  val loginCookieName: String = if cookieSecure then "__Host-skereru_login" else "skereru_login"
  val publicOrigin: String = scala.util.Try { val u = java.net.URI.create(publicUrl); s"${u.getScheme}://${u.getAuthority}" }.getOrElse(publicUrl)

/**
 * 認可を求める外部のサービス（OAUTH_CLIENTS で登録する）。redirectUri は完全一致で照合する。
 * scopes はそのクライアントが求めてよいスコープ（read・write・send. 既定は read だけ. admin は付けられない）。
 */
final case class OAuthClient(id: String, secret: String, redirectUri: String, name: String, scopes: List[String] = List(ApiScope.Read))

object OAuthClient:
  /** OAuth で許せるスコープ（メールの送信は影響が大きいので、send は write と別にする）。 */
  val Scopes: List[String] = List(ApiScope.Read, ApiScope.Write, ApiScope.Send)

/** 処理の失敗（HTTP の応答に変換する）。 */
enum Fail:
  case NotFound(msg: String)
  case Forbidden(msg: String)
  case Invalid(msg: String)
  case Conflict(msg: String)
  case TooLarge(msg: String)

  def message: String = this match
    case NotFound(m)  => m
    case Forbidden(m) => m
    case Invalid(m)   => m
    case Conflict(m)  => m
    case TooLarge(m)  => m

object Fail:
  def from(e: StoreError): Fail = e match
    case StoreError.NotFound(w) => Fail.NotFound(s"$w が見つかりません")
    case StoreError.Invalid(m)  => Fail.Invalid(m)
    case StoreError.Conflict(m) => Fail.Conflict(m)

type Res[A] = IO[Either[Fail, A]]

object Op:
  def zone(tz: String): java.time.ZoneId = scala.util.Try(java.time.ZoneId.of(tz)).getOrElse(java.time.ZoneId.of("Asia/Tokyo"))

extension [A](io: IO[Either[StoreError, A]]) def orFail: Res[A] = io.map(_.left.map(Fail.from))

/** APIキーの発行（自分のキーと、管理者が発行するエージェント専用アカウントのキーで共通）。平文のキーは応答で1回だけ返す。 */
object ApiKeyIssuer:
  /** adminDenied: admin スコープを付けられないときの理由（付けられるなら None）。 */
  def issue(users: UserStore, userId: UUID, r: CreateApiKeyReq, adminDenied: Option[Fail]): Res[CreatedApiKeyDto] =
    val scopes = r.scopes.map(_.trim.toLowerCase).distinct
    val invalid: Option[Fail] =
      if r.name.trim.isEmpty then Some(Fail.Invalid("名前を入力してください"))
      else if scopes.isEmpty || !scopes.forall(ApiScope.all.contains) then Some(Fail.Invalid("スコープは read / write / send / admin から選んでください"))
      else if scopes.contains(ApiScope.Admin) then adminDenied
      else if r.expiresDays.exists(d => d < 1 || d > 3650) then Some(Fail.Invalid("有効期限は1〜3650日です"))
      else None
    invalid match
      case Some(f) => IO.pure(Left(f))
      case None =>
        val (key, prefix) = ApiKeys.generate()
        users.createApiKey(userId, r.name.trim.take(100), Crypto.sha256(key), prefix, scopes,
          r.expiresDays.map(d => Instant.now().plusSeconds(d.toLong * 86400))).map(info => Right(CreatedApiKeyDto(key, info)))

/** 認証: Cookie のセッション、または `Authorization: Bearer <APIキー>`。 */
final class SessionAuth(users: UserStore, cfg: ApiConfig):

  private def isAdmin(u: User, gs: List[String]) = u.isAdmin || gs.exists(cfg.adminGroups.contains)

  /** APIキーでは、グループによる管理者の権限は、最近ログインして（グループを同期して）いるときだけ認める。
    * IdP で管理者のグループから外しても、ログインしない限りこちらのグループは古いままなので。 */
  private def isAdminForKey(u: User, gs: List[String], now: Instant) =
    u.isAdmin || (gs.exists(cfg.adminGroups.contains) && u.lastLoginAt.exists(_.isAfter(now.minusSeconds(cfg.adminGroupFreshness.toSeconds))))

  def fromRequest(req: Request[IO]): IO[Option[AuthSession]] =
    bearer(req) match
      case Some(token) =>
        val now = Instant.now()
        users.findApiKey(Crypto.sha256(token), now).map(_.map { (u, gs, kid, scopes) =>
          val mcp = req.pathInfo.renderString.startsWith("/mcp") || req.headers.get(CIString("X-Skereru-Mcp")).isDefined
          AuthSession(u, gs, isAdminForKey(u, gs, now), AuthVia.ApiKey(kid, scopes.toSet, mcp))
        })
      case None =>
        req.cookies.find(_.name == cfg.cookieName).map(_.content) match
          case None        => IO.pure(None)
          case Some(token) => users.findSession(Crypto.sha256(token), Instant.now()).map(_.map((u, gs) => AuthSession(u, gs, isAdmin(u, gs))))

  private def bearer(req: Request[IO]): Option[String] =
    req.headers.get[Authorization].collect { case Authorization(Credentials.Token(AuthScheme.Bearer, t)) => t }.filter(ApiKeys.looksLike)

  /** セッションを発行する。管理者のセッションは短くする（IdP での権限の取り消しを早く反映するため）。 */
  def issue(user: User, req: Request[IO], idToken: Option[String] = None): IO[ResponseCookie] =
    val token = Crypto.randomToken()
    users.groupsIO(user.id).flatMap { gs =>
      val ttl = if isAdmin(user, gs) then cfg.adminSessionTtl else cfg.sessionTtl
      users.createSession(Crypto.sha256(token), user.id, Instant.now().plusSeconds(ttl.toSeconds), Some(ReqInfo.clientIp(req)),
        req.headers.get(CIString("User-Agent")).map(_.head.value), idToken).as(cookie(token, ttl.toSeconds))
    }

  /** セッションを消す. IdP からもログアウトするための id_token（OIDC でログインしていたとき）と、Cookie を消す指示を返す. */
  def revoke(req: Request[IO]): IO[(Option[String], ResponseCookie)] =
    req.cookies.find(_.name == cfg.cookieName) match
      case Some(c) => users.deleteSession(Crypto.sha256(c.content)).map(_ -> cookie("", 0))
      case None    => IO.pure(None -> cookie("", 0))

  private def cookie(value: String, maxAge: Long): ResponseCookie =
    ResponseCookie(cfg.cookieName, value, maxAge = Some(maxAge), path = Some("/"), httpOnly = true, secure = cfg.cookieSecure, sameSite = Some(SameSite.Lax))

object ReqInfo:
  private val Key = org.typelevel.vault.Key.newKey[SyncIO, String].unsafeRunSync()

  /** 接続元の IP（middleware が決めた値。なければ直接の接続元）。 */
  def clientIp(req: Request[IO]): String =
    req.attributes.lookup(Key).orElse(req.remoteAddr.map(_.toString)).getOrElse("-")

  /** 直接の接続元が trusted（ロードバランサー）のときだけ、X-Forwarded-For の最後の値（ロードバランサーが追記した値）を信じる。 */
  def middleware(trusted: List[(java.net.InetAddress, Int)])(app: HttpApp[IO]): HttpApp[IO] =
    cats.data.Kleisli { req =>
      val direct = req.remoteAddr.map(_.toString)
      val fromProxy = req.remoteAddr.exists(a => scala.util.Try(java.net.InetAddress.getByName(a.toString)).toOption
        .exists(ip => trusted.exists((net, bits) => skereru.core.Cidr.contains(net, bits, ip))))
      val ip = (if fromProxy then req.headers.get(CIString("X-Forwarded-For")).map(_.head.value.split(',').last.trim).filter(_.nonEmpty) else None)
        .orElse(direct).getOrElse("-")
      app(req.withAttribute(Key, ip))
    }

/** *Routes に共通の補助（エラー応答・認証・スコープ・CSRF・リクエスト本文）。 */
trait RouteSupport extends Http4sDsl[IO]:
  protected def sessions: SessionAuth

  protected def err(status: Status, msg: String): IO[Response[IO]] = Response[IO](status).withEntity(ErrorDto(msg)).pure[IO]

  protected def fail(f: Fail): IO[Response[IO]] = f match
    case Fail.NotFound(m)  => err(Status.NotFound, m)
    case Fail.Forbidden(m) => err(Status.Forbidden, m)
    case Fail.Invalid(m)   => err(Status.BadRequest, m)
    case Fail.Conflict(m)  => err(Status.Conflict, m)
    case Fail.TooLarge(m)  => err(Status.PayloadTooLarge, m)

  protected def respond[A](r: Res[A])(f: A => IO[Response[IO]]): IO[Response[IO]] = r.flatMap(_.fold(fail, f))
  protected def okJson[A: io.circe.Encoder](r: Res[A]): IO[Response[IO]] = respond(r)(a => Ok(a))

  private val safe = Set(Method.GET, Method.HEAD, Method.OPTIONS)

  /** ログインしていなければ 401。スコープが足りなければ 403。
    * Cookie で更新系の要求をするときは X-Skereru ヘッダーを必須にする（ほかのサイトからは付けられないので CSRF を防げる）。 */
  protected def authed(req: Request[IO], scope: String = "")(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    sessions.fromRequest(req).flatMap {
      case None => err(Status.Unauthorized, "ログインしてください")
      case Some(s) =>
        val need = if scope.nonEmpty then scope else if safe.contains(req.method) then ApiScope.Read else ApiScope.Write
        // 操作したユーザーを、トレース（スパンの属性）とこの中のログに載せる
        skereru.observe.Telemetry.withUser(s.user.id.toString, s.user.name) {
          if s.isSession && !safe.contains(req.method) && req.headers.get(CIString("X-Skereru")).isEmpty then
            err(Status.Forbidden, "不正なリクエストです（X-Skereru ヘッダーがありません）")
          else if !s.can(need) then
            err(Status.Forbidden, if need == ApiScope.Admin then "管理者だけが使えます" else s"このAPIキーには $need のスコープがありません")
          else f(s)
        }
    }

  protected def admin(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] = authed(req, ApiScope.Admin)(f)

  /** Cookie のセッションのみ（APIキー・アプリパスワードの発行など）。 */
  protected def sessionOnly(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] = authed(req)(onlySession(f))

  /** 管理者の、Cookie のセッションのみ（APIキーでエージェント専用アカウントを管理させない）。 */
  protected def adminSessionOnly(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] = admin(req)(onlySession(f))

  private def onlySession(f: AuthSession => IO[Response[IO]])(s: AuthSession): IO[Response[IO]] =
    if s.isSession then f(s) else err(Status.Forbidden, "この操作は画面からのみ行えます")

  protected def body[A: Decoder](req: Request[IO])(f: A => IO[Response[IO]]): IO[Response[IO]] =
    req.as[A].attempt.flatMap {
      case Left(e)  => err(Status.BadRequest, s"リクエストの内容が正しくありません: ${e.getMessage.take(200)}")
      case Right(b) => f(b)
    }

  protected object UuidVar:
    def unapply(s: String): Option[UUID] = scala.util.Try(UUID.fromString(s)).toOption

  protected def param(req: Request[IO], k: String): Option[String] = req.params.get(k).map(_.trim).filter(_.nonEmpty)

  protected def intParam(req: Request[IO], k: String, default: Int, max: Int): Int =
    param(req, k).flatMap(_.toIntOption).map(v => math.max(0, math.min(v, max))).getOrElse(default)

  /** multipart/form-data の file を受け取る（上限を超えたら 413）。 */
  protected def withUpload(req: Request[IO], maxBytes: Long)(f: (String, String, Array[Byte]) => IO[Response[IO]]): IO[Response[IO]] =
    req.as[org.http4s.multipart.Multipart[IO]].attempt.flatMap {
      case Left(_) => err(Status.BadRequest, "multipart/form-data で file を送ってください")
      case Right(mp) =>
        mp.parts.find(_.name.contains("file")) match
          case None => err(Status.BadRequest, "file がありません")
          case Some(part) =>
            val name = part.filename.map(_.trim).filter(_.nonEmpty).getOrElse("file").replaceAll("[\\\\/\\r\\n\\u0000]", "_").take(200)
            val ct = part.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}").getOrElse("application/octet-stream")
            part.body.take(maxBytes + 1).compile.to(Array).flatMap { bytes =>
              if bytes.length > maxBytes then err(Status.PayloadTooLarge, s"ファイルが大きすぎます（上限 ${math.max(1, maxBytes / 1024 / 1024)}MB）")
              else f(name, ct, bytes)
            }
    }

  protected def audit(users: UserStore, s: AuthSession, req: Request[IO], action: String, target: Option[String], detail: Json = Json.obj()): IO[Unit] =
    users.audit(Some(s.user.id), s.viaString, action, target, Some(ReqInfo.clientIp(req)), detail)
