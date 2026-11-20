package skereru.api

import cats.effect.*
import sbirds.shared.AppError
import skereru.shared.*

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
  oauthClients: List[OAuthClient] = Nil) {

  /** https なら __Host- の付いた Cookie（Secure・Path=/・Domain なしが強制される）。 */
  val cookieName: String = if cookieSecure then "__Host-skereru_session" else "skereru_session"

  /** ログインの途中（IdP へ行って戻るまで）の state を入れる Cookie。戻ってきたブラウザが、ログインを始めたブラウザと同じか確かめる。 */
  val loginCookieName: String = if cookieSecure then "__Host-skereru_login" else "skereru_login"
  val publicOrigin: String = scala.util.Try {
    val u = java.net.URI.create(publicUrl); s"${u.getScheme}://${u.getAuthority}"
  }.getOrElse(publicUrl)
}

/**
 * 認可を求める外部のサービス（OAUTH_CLIENTS で登録する。sbirds.oauth.OAuthClient）。
 * scopes はそのクライアントが求めてよいスコープ（read・write・send. 既定は read だけ. admin は付けられない）。
 */
type OAuthClient = sbirds.oauth.OAuthClient
val OAuthClient = sbirds.oauth.OAuthClient

object OAuthScopes {

  /** OAuth で許せるスコープ（メールの送信は影響が大きいので、send は write と別にする）。 */
  val All: List[String] = List(ApiScope.Read, ApiScope.Write, ApiScope.Send)
}

/** 処理の結果（Left はエラーコード付きの失敗. 応答は RouteSupport.fail）。 */
type Res[A] = IO[Either[AppError, A]]

object Op {
  def zone(tz: String): java.time.ZoneId =
    scala.util.Try(java.time.ZoneId.of(tz)).getOrElse(java.time.ZoneId.of("Asia/Tokyo"))
}
