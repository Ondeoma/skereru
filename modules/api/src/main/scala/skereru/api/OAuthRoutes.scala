package skereru.api

import skereru.core.ErrorCode

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skereru.auth.ApiKeys
import sbirds.auth.Crypto
import sbirds.oauth.{OAuthCode, OAuthProvider}
import skereru.core.Html
import skereru.db.*
import skereru.shared.*

import java.time.Instant
import java.util.UUID

/**
 * 外部のサービス（skakapo など）への認可（OAuth 2.0 の認可コード + PKCE。流れは sbirds.oauth.OAuthProvider）。
 * アクセストークンは、そのユーザーの有効期限つきの API キー（スコープは許可したもの）。refresh token で更新する（使うたびに新しくする）。
 * 利用者は設定の「APIキー・MCP」の「連携しているサービス」から、いつでも取り消せる。
 */
final class OAuthRoutes(oauth: OAuthStore, users: UserStore, val sessions: SessionAuth, cfg: ApiConfig)
  extends RouteSupport {

  private val TokenTtlSeconds = 3600L

  private val host = new OAuthProvider.Host[AuthSession] {
    val settings = OAuthProvider.Settings(
      "skereru",
      cfg.cookieSecure,
      invalidScopeDescription = Some("このクライアントに許していないスコープです"),
      grantTypeMissing = Some("grant_type を指定してください")
    )
    val codes = oauth

    def client(id: String): IO[Option[OAuthClient]] = IO.pure(OAuthRoutes.this.client(id))

    // 同意は画面のログイン（Cookie のセッション）だけ
    def browserSession(req: Request[IO]): IO[Option[AuthSession]] = sessions.fromRequest(req).map(_.filter(_.isSession))

    def userId(s: AuthSession): UUID = s.user.id

    /** 求められたスコープ（なければ read）。クライアントに許していないものが含まれていれば None。 */
    override def scopes(c: OAuthClient, requested: Option[String]): Option[List[String]] =
      OAuthProvider.requestedScopes(c.scopes, requested, List(ApiScope.Read)).map(rs =>
        OAuthScopes.All.filter(rs.contains)
      )

    def consent(req: Request[IO], c: OAuthClient, s: AuthSession, sc: List[String]): IO[OAuthProvider.Consent] = {
      val name = Html.escape(c.name)
      IO.pure(OAuthProvider.Consent(
        s"$name との連携",
        s"""<h1>$name との連携</h1>
           |<p><b>$name</b> が、あなた（${Html.escape(s.user.name)}）の権限で skereru の次のことを行います。</p>
           |<ul>${sc.map(scopeItem).mkString}</ul>
           |<p class="muted">設定の「APIキー・MCP」の「連携しているサービス」から、いつでも連携をやめられます。</p>""".stripMargin
      ))
    }

    /**
     * 許可の画面。CSP の form-action は付けない（フォームの送信後のリダイレクトの連鎖にも効くため、
     * 連携元が callback の後に別の origin へ戻すと止まる）。CSRF は Cookie と hidden の値の照合で防ぐ。
     */
    override def page(title: String, body: String, status: Status): IO[Response[IO]] = {
      val html =
        s"""<!DOCTYPE html><html lang="ja"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1">
           |<title>${Html.escape(title)} | skereru</title><style>
           |:root{color-scheme:light dark}body{margin:0;font-family:system-ui,-apple-system,"Hiragino Sans","Noto Sans JP",sans-serif;background:#f1f5f9;color:#1e293b;
           |display:flex;min-height:100vh;align-items:center;justify-content:center;padding:1rem;box-sizing:border-box}
           |@media(prefers-color-scheme:dark){body{background:#0f172a;color:#e2e8f0}.card{background:#1c273d!important;border-color:#283548!important}.send{background:rgba(248,113,113,.12)!important}}
           |.card{background:#fff;border:1px solid #e2e8f0;border-radius:14px;padding:1.6rem 1.8rem;max-width:460px;width:100%;box-sizing:border-box;
           |box-shadow:0 11px 15px -7px rgba(0,0,0,.2),0 24px 38px 3px rgba(0,0,0,.14)}
           |h1{font-size:1.15rem;margin:0 0 .8rem}p{line-height:1.7;margin:.4rem 0}.muted{color:#64748b;font-size:.85rem}.logo{color:#2563eb;font-weight:800}
           |.actions{display:flex;gap:.6rem;justify-content:flex-end;margin-top:1.2rem}button{font:inherit;border-radius:8px;padding:.5rem 1.1rem;cursor:pointer;
           |border:1px solid #cbd5e1;background:transparent;color:inherit}button.primary{background:#2563eb;border-color:#2563eb;color:#fff}
           |ul{padding-left:1.2rem;margin:.4rem 0}li{margin:.3rem 0;line-height:1.6}code{font-size:.8rem;color:#64748b}
           |.send{background:rgba(220,38,38,.08);border-radius:6px;padding:.2rem .4rem}.send b{color:#dc2626}</style></head>
           |<body><div class="card"><div class="logo">skereru</div>$body</div></body></html>""".stripMargin
      IO.pure(OAuthProvider.htmlResponse(html, status))
    }

    override def authorized(req: Request[IO], s: AuthSession, c: OAuthClient, sc: List[String]): IO[Unit] =
      audit(users, s, req, "oauth.authorize", Some(c.id), Json.obj("scopes" -> sc.asJson))

    def issue(req: Request[IO], c: OAuthClient, oc: OAuthCode): IO[OAuthProvider.Token] = {
      val sc = OAuthScopes.All.filter(x => oc.scope.split(' ').contains(x) && c.scopes.contains(x))
      val (key, prefix, refresh) = newTokens()
      oauth.grant(
        oc.userId,
        c.id,
        sc,
        Crypto.sha256(refresh),
        keyName(c),
        prefix,
        Crypto.sha256(key),
        Instant.now().plusSeconds(TokenTtlSeconds)
      ) *>
        users.audit(
          Some(oc.userId),
          "oauth",
          "oauth.token",
          Some(c.id),
          Some(ReqInfo.clientIp(req)),
          Json.obj("scopes" -> sc.asJson)
        ).as(token(key, refresh, sc))
    }

    override def refresh: Option[OAuthProvider.Refresh] = Some { (_, c, rt, _) =>
      val (key, prefix, refresh) = newTokens()
      oauth.refresh(
        c.id,
        Crypto.sha256(rt),
        Crypto.sha256(refresh),
        keyName(c),
        prefix,
        Crypto.sha256(key),
        Instant.now().plusSeconds(TokenTtlSeconds)
      )(_.filter(c.scopes.contains)).map {
        case Some(g) => Right(token(key, refresh, g.scopes))
        case None => Left(OAuthProvider.Failure.invalidGrant("refresh_token が無効です（取り消し済み・使用済み）"))
      }
    }

    def revoke(req: Request[IO], c: OAuthClient, token: String): IO[Unit] =
      oauth.revokeByToken(Crypto.sha256(token), c.id)
  }

  val routes: HttpRoutes[IO] = new OAuthProvider(host).routes <+> HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "me" / "oauth-grants" => getMeOauthGrants(req)
    case req @ DELETE -> Root / "api" / "me" / "oauth-grants" / clientId => deleteMeOauthGrant(req, clientId)
  }

  private def getMeOauthGrants(req: Request[IO]): IO[Response[IO]] =
    sessionOnly(req)(s =>
      oauth.grants(s.user.id).flatMap(gs =>
        Ok(gs.map {
          (id, sc, at, used) =>
            OAuthGrantDto(id, client(id).map(_.name).getOrElse(id), sc, at, used)
        })
      )
    )

  private def deleteMeOauthGrant(req: Request[IO], clientId: String): IO[Response[IO]] =
    sessionOnly(req)(s =>
      oauth.revoke(s.user.id, clientId).flatMap(ok =>
        if !ok then err(ErrorCode.OAuthGrantNotFound)
        else audit(users, s, req, "oauth.revoke", Some(clientId)) *> NoContent()
      )
    )

  private def client(id: String): Option[OAuthClient] = cfg.oauthClients.find(_.id == id)

  /** 許可の画面に出す、スコープの説明。send は「メールを送信できる」と目立たせる。 */
  private def scopeItem(sc: String): String = sc match {
    case ApiScope.Read =>
      "<li><b>メールを読む</b> <code>read</code><br><span class=\"muted\">メールボックス・メッセージ・添付ファイル・連絡先・設定を見ます</span></li>"
    case ApiScope.Write =>
      "<li><b>メールを整理する</b> <code>write</code><br><span class=\"muted\">既読にする・移動・削除（ゴミ箱）・振り分けの規則や設定を変えます</span></li>"
    case ApiScope.Send =>
      "<li class=\"send\"><b>メールを送信できる</b> <code>send</code><br><span class=\"muted\">あなたのアドレスから、あなたの名前でメールを送信・予約送信します（転送の規則・自動返信も設定できます）</span></li>"
    case other => s"<li>${Html.escape(other)}</li>"
  }

  /** 新しいアクセストークン（API キー）と refresh token。 */
  private def newTokens(): (String, String, String) = {
    val (key, prefix) = ApiKeys.generate()
    (key, prefix, "skrr_" + Crypto.randomToken(32))
  }

  private def token(access: String, refresh: String, scopes: List[String]): OAuthProvider.Token =
    OAuthProvider.Token(access, scopes.mkString(" "), Some(TokenTtlSeconds), Some(refresh))

  private def keyName(c: OAuthClient) = s"${c.name} 連携"

}
