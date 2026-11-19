package skereru.auth

import cats.syntax.all.*
import io.circe.Json
import io.circe.parser.parse

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.Base64

object IdToken {

  /**
   * JWTのペイロードを取り出す（署名は検証しない）。
   * IDトークンはトークンエンドポイントからTLS経由で直接受け取るため、OIDC Core 3.1.3.7 に従い
   * TLSサーバー認証でIssuerを検証できる場合は署名検証を省略できる。
   * 代わりに iss / aud / exp / nonce は必ず検証する。
   */
  def decodeClaims(
    jwt: String
  ): Either[String, Json] =
    jwt.split('.') match {
      case Array(_, payload, _*) =>
        Either
          .catchNonFatal(new String(Base64.getUrlDecoder.decode(payload), StandardCharsets.UTF_8))
          .leftMap(_ => "invalid base64 in id_token")
          .flatMap(parse(_).leftMap(e => s"invalid json in id_token: ${e.getMessage}"))
      case _ => Left("malformed id_token")
    }

  def validate(
    claims: Json,
    issuer: String,
    cfg: OidcConfig,
    expectedNonce: String,
    now: Instant
  ): Either[String, Identity] = {
    val c = claims.hcursor
    for {
      iss <- c.get[String]("iss").leftMap(_ => "missing iss")
      _ <- Either.cond(iss == issuer, (), s"issuer mismatch: $iss")
      aud <- c.downField("aud").focus.toRight("missing aud").flatMap { j =>
        j.asString.map(List(_)).orElse(j.asArray.map(_.toList.flatMap(_.asString))).toRight("invalid aud")
      }
      _ <- Either.cond(aud.contains(cfg.clientId), (), "audience mismatch")
      exp <- c.get[Long]("exp").leftMap(_ => "missing exp")
      _ <- Either.cond(Instant.ofEpochSecond(exp).plusSeconds(60).isAfter(now), (), "id_token expired")
      non <- c.get[String]("nonce").leftMap(_ => "missing nonce")
      _ <- Either.cond(non == expectedNonce, (), "nonce mismatch")
      sub <- c.get[String]("sub").leftMap(_ => "missing sub")
    } yield {
      val email = c.get[String]("email").toOption
      val name = c.get[String]("name").toOption.filter(_.nonEmpty)
        .orElse(c.get[String]("preferred_username").toOption)
        .orElse(email)
        .getOrElse(sub)
      val groups = c.downField(cfg.groupsClaim).as[List[String]].getOrElse(Nil)
      val verified = c.get[Boolean]("email_verified").toOption
        .orElse(c.get[String]("email_verified").toOption.map(_.equalsIgnoreCase("true")))
        .getOrElse(cfg.trustEmail)
      Identity(sub, email, name, groups, c.get[String]("picture").toOption, verified)
    }
  }
}
