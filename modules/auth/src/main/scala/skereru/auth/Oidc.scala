package skereru.auth

import org.http4s.*

/*
 * OIDC のクライアント. sbirds-lib の auth（OidcClient）には email_verified（Identity.emailVerified）と
 * OIDC_TRUST_EMAIL（OidcConfig.trustEmail）がないため、skereru ではこのファイルを残す（アカウントの結び付けに必要）.
 */
final case class OidcConfig(
  issuer: String,
  clientId: String,
  clientSecret: String,
  redirectUri: String,
  groupsClaim: String = "groups",
  /** email_verified がない IdP（Microsoft Entra ID など）でも、メールアドレスを確かめ済みとして扱う。 */
  trustEmail: Boolean = false,
  scopes: List[String] = List("openid", "profile", "email"))

/** emailVerified: IdP がメールアドレスを確かめたか（アカウントの結び付け・メールボックスの自動作成はこれが true のときだけ）。 */
final case class Identity(sub: String,
                          email: Option[String],
                          name: String,
                          groups: List[String],
                          picture: Option[String] = None,
                          emailVerified: Boolean = false,
                          idToken: Option[String] = None)

final case class ProviderMetadata(issuer: String,
                                  authorizationEndpoint: Uri,
                                  tokenEndpoint: Uri,
                                  endSessionEndpoint: Option[Uri] = None)
