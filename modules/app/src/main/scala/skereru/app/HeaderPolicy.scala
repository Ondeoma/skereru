package skereru.app

import sbirds.http.SecurityHeaders
import sbirds.observe.RumRoutes
import skereru.api.*

/** すべての応答に付けるセキュリティのヘッダー（ルートが付けたものは上書きしない）。 */
object HeaderPolicy {
  // IdP が http（開発環境など）のときは、そのアイコンの画像も許す（https の画像はもとから許している）
  private val idpImg: String = sys.env.get("OIDC_ISSUER").flatMap(u => scala.util.Try(java.net.URI.create(u)).toOption)
    .filter(u => u.getScheme == "http" && u.getAuthority != null).fold("")(u => s" http://${u.getAuthority}")
  // 画面のブラウザの観測（skorimako の RUM）の設定があれば、その SDK の読み込みと送信を許す
  private val rum = RumRoutes.origin.fold("")(" " + _)
  private val csp =
    s"default-src 'self'; script-src 'self'$rum; style-src 'self' 'unsafe-inline'; img-src 'self' data: blob: https:$idpImg; font-src 'self' data:; " +
      s"connect-src 'self'$rum; frame-src 'self'; frame-ancestors 'self'; object-src 'none'; base-uri 'self'; form-action 'self'"

  def apply(cfg: ApiConfig): SecurityHeaders.Policy = SecurityHeaders.Policy(
    hsts = SecurityHeaders.hsts(cfg.cookieSecure, "max-age=63072000; includeSubDomains"),
    frameOptions = Some("SAMEORIGIN"),
    referrer = Some("same-origin"),
    csp = Some(csp),
    extra = List(
      "Permissions-Policy" -> "camera=(), microphone=(), geolocation=(), payment=(), usb=()",
      "Cross-Origin-Opener-Policy" -> "same-origin"
    ),
    keepExisting = true
  )
}
