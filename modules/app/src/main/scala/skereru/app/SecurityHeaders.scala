package skereru.app

import cats.effect.*
import org.http4s.*
import org.typelevel.ci.CIString
import skereru.api.*
import sbirds.observe.RumRoutes

/** すべての応答に付けるセキュリティのヘッダー（ルートが付けたものは上書きしない）。 */
object SecurityHeaders {
  // IdP が http（開発環境など）のときは、そのアイコンの画像も許す（https の画像はもとから許している）
  private val idpImg: String = sys.env.get("OIDC_ISSUER").flatMap(u => scala.util.Try(java.net.URI.create(u)).toOption)
    .filter(u => u.getScheme == "http" && u.getAuthority != null).fold("")(u => s" http://${u.getAuthority}")
  // 画面のブラウザの観測（skorimako の RUM）の設定があれば、その SDK の読み込みと送信を許す
  private val rum = RumRoutes.origin.fold("")(" " + _)
  private val csp =
    s"default-src 'self'; script-src 'self'$rum; style-src 'self' 'unsafe-inline'; img-src 'self' data: blob: https:$idpImg; font-src 'self' data:; " +
      s"connect-src 'self'$rum; frame-src 'self'; frame-ancestors 'self'; object-src 'none'; base-uri 'self'; form-action 'self'"

  def apply(cfg: ApiConfig)(app: HttpApp[IO]): HttpApp[IO] = cats.data.Kleisli { req =>
    app.run(req).map { r =>
      val add = List(
        Option.when(cfg.cookieSecure)("Strict-Transport-Security" -> "max-age=63072000; includeSubDomains"),
        Some("X-Content-Type-Options" -> "nosniff"),
        Some("Referrer-Policy" -> "same-origin"),
        Some("X-Frame-Options" -> "SAMEORIGIN"),
        Some("Permissions-Policy" -> "camera=(), microphone=(), geolocation=(), payment=(), usb=()"),
        Some("Cross-Origin-Opener-Policy" -> "same-origin"),
        Some("Content-Security-Policy" -> csp)
      ).flatten.filterNot((k, _) => r.headers.get(CIString(k)).isDefined)
      r.putHeaders(add.map(h => Header.ToRaw.keyValuesToRaw(h))*)
    }
  }
}
