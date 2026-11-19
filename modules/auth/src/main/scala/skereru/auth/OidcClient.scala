package skereru.auth

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import sbirds.auth.Crypto

import java.time.Instant

/** OIDCプロバイダ向けクライアント（Authorization Code + PKCE）。 */
final class OidcClient private (cfg: OidcConfig, http: Client[IO], meta: Ref[IO, Option[ProviderMetadata]]) {

  private def discover: IO[ProviderMetadata] = meta.get.flatMap {
    case Some(m) => IO.pure(m)
    case None =>
      val url = Uri.unsafeFromString(cfg.issuer.stripSuffix("/") + "/.well-known/openid-configuration")
      http.expect[Json](url).flatMap { j =>
        val c = j.hcursor
        val res = for {
          iss <- c.get[String]("issuer")
          auth <- c.get[String]("authorization_endpoint")
          tok <- c.get[String]("token_endpoint")
          a <- Uri.fromString(auth).leftMap(e => io.circe.DecodingFailure(e.message, Nil))
          t <- Uri.fromString(tok).leftMap(e => io.circe.DecodingFailure(e.message, Nil))
        } yield ProviderMetadata(
          iss,
          a,
          t,
          c.get[String]("end_session_endpoint").toOption.flatMap(Uri.fromString(_).toOption)
        )
        IO.fromEither(res).flatTap(m => meta.set(Some(m)))
      }
  }

  def authorizationUri(state: String, verifier: String, nonce: String): IO[Uri] =
    discover.map { m =>
      m.authorizationEndpoint.withQueryParams(
        Map(
          "response_type" -> "code",
          "client_id" -> cfg.clientId,
          "redirect_uri" -> cfg.redirectUri,
          "scope" -> cfg.scopes.mkString(" "),
          "state" -> state,
          "nonce" -> nonce,
          "code_challenge" -> Crypto.pkceChallenge(verifier),
          "code_challenge_method" -> "S256"
        )
      )
    }

  /** 認可コードをIDトークンに交換し、検証済みの Identity を返す。 */
  def authenticate(code: String, verifier: String, nonce: String, now: Instant): IO[Identity] =
    for {
      m <- discover
      req = Request[IO](Method.POST, m.tokenEndpoint).withEntity(
        UrlForm(
          "grant_type" -> "authorization_code",
          "code" -> code,
          "redirect_uri" -> cfg.redirectUri,
          "client_id" -> cfg.clientId,
          "client_secret" -> cfg.clientSecret,
          "code_verifier" -> verifier
        )
      )
      resp <- http.expect[Json](req)
      idToken <-
        IO.fromEither(resp.hcursor.get[String]("id_token").leftMap(e => new RuntimeException(s"no id_token: $e")))
      claims <- IO.fromEither(IdToken.decodeClaims(idToken).leftMap(new RuntimeException(_)))
      id <- IO.fromEither(IdToken.validate(claims, m.issuer, cfg, nonce, now).leftMap(new RuntimeException(_)))
    } yield id.copy(idToken = Some(idToken))

  /** IdP からもログアウトする URL（end_session_endpoint がなければ None）. */
  def logoutUri(idToken: Option[String], postLogout: String): IO[Option[Uri]] =
    discover.attempt.map(_.toOption.flatMap(_.endSessionEndpoint).map { e =>
      e.withQueryParams(Map("post_logout_redirect_uri" -> postLogout, "client_id" -> cfg.clientId) ++
        idToken.map("id_token_hint" -> _))
    })
}

object OidcClient {
  def create(cfg: OidcConfig, http: Client[IO]): IO[OidcClient] =
    Ref.of[IO, Option[ProviderMetadata]](None).map(new OidcClient(cfg, http, _))
}
