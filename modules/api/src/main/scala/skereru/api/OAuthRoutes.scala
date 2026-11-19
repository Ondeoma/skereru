package skereru.api

import skereru.core.ErrorCode

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.{`Content-Type`, Authorization, Location}
import skereru.auth.ApiKeys
import sbirds.auth.Crypto
import skereru.core.Html
import skereru.db.*
import skereru.shared.*

import java.time.Instant

/**
 * 外部のサービス（skakapo など）への認可（OAuth 2.0 の認可コード + PKCE。RFC 6749・7636・7009）。
 * アクセストークンは、そのユーザーの有効期限つきの API キー（スコープは許可したもの）。refresh token で更新する（使うたびに新しくする）。
 * 利用者は設定の「APIキー・MCP」の「連携しているサービス」から、いつでも取り消せる。
 */
final class OAuthRoutes(oauth: OAuthStore, users: UserStore, val sessions: SessionAuth, cfg: ApiConfig)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "oauth" / "authorize" => getOauthAuthorize(req)
    case req @ POST -> Root / "oauth" / "authorize" => postOauthAuthorize(req)
    case req @ POST -> Root / "oauth" / "token" => postOauthToken(req)
    case req @ POST -> Root / "oauth" / "revoke" => postOauthRevoke(req)
    case req @ GET -> Root / "api" / "me" / "oauth-grants" => getMeOauthGrants(req)
    case req @ DELETE -> Root / "api" / "me" / "oauth-grants" / clientId => deleteMeOauthGrant(req, clientId)
  }

  private def getOauthAuthorize(req: Request[IO]): IO[Response[IO]] =
    validate(param(req, _)) match {
      case Left(r) => r
      case Right((c, _, _, sc)) =>
        sessions.fromRequest(req).flatMap {
          case Some(s) if s.isSession =>
            val csrf = Crypto.randomToken(24)
            val hidden = List(
              "client_id",
              "redirect_uri",
              "state",
              "code_challenge",
              "code_challenge_method",
              "response_type",
              "scope"
            )
              .flatMap(k =>
                param(req, k).map(v => s"""<input type="hidden" name="$k" value="${Html.escape(v)}">""")
              ).mkString
            val name = Html.escape(c.name)
            page(
              s"$name との連携",
              s"""<h1>$name との連携</h1>
                 |<p><b>$name</b> が、あなた（${Html.escape(s.user.name)}）の権限で skereru の次のことを行います。</p>
                 |<ul>${sc.map(scopeItem).mkString}</ul>
                 |<p class="muted">設定の「APIキー・MCP」の「連携しているサービス」から、いつでも連携をやめられます。</p>
                 |<form method="post" action="/oauth/authorize">$hidden<input type="hidden" name="csrf" value="$csrf">
                 |<div class="actions"><button type="submit" name="decision" value="deny">拒否する</button>
                 |<button type="submit" name="decision" value="allow" class="primary">許可する</button></div></form>""".stripMargin
            )
              .map(_.addCookie(ResponseCookie(
                CsrfCookie,
                csrf,
                path = Some("/oauth"),
                httpOnly = true,
                secure = cfg.cookieSecure,
                sameSite = Some(SameSite.Strict),
                maxAge = Some(600)
              )))
          case _ =>
            // ログインしてからこの画面に戻る
            IO.pure(Response[IO](
              Status.Found
            ).putHeaders(Location(Uri.unsafeFromString(s"/auth/login?returnTo=${enc(req.uri.renderString)}"))))
        }
    }

  private def postOauthAuthorize(req: Request[IO]): IO[Response[IO]] =
    req.as[UrlForm].flatMap { u =>
      val p = (k: String) => u.getFirst(k).map(_.trim).filter(_.nonEmpty)
      validate(p) match {
        case Left(r) => r
        case Right((c, state, challenge, sc)) =>
          val csrfOk = req.cookies.find(_.name == CsrfCookie).exists(ck =>
            p("csrf").exists(v => Crypto.constantTimeEquals(ck.content, v))
          )
          sessions.fromRequest(req).flatMap {
            case Some(s) if !s.isSession => errorPage("ログインが切れました。連携元の画面からやり直してください")
            case None => errorPage("ログインが切れました。連携元の画面からやり直してください")
            case Some(_) if !csrfOk => errorPage("画面の有効期限が切れました。連携元の画面からやり直してください")
            case Some(s) if p("decision").contains("allow") =>
              val code = Crypto.randomToken(32)
              oauth.createCode(
                Crypto.sha256(code),
                OAuthCode(c.id, c.redirectUri, s.user.id, challenge, sc.mkString(" ")),
                Instant.now().plusSeconds(CodeTtlSeconds)
              ) *>
                audit(users, s, req, "oauth.authorize", Some(c.id), Json.obj("scopes" -> sc.asJson)) *>
                back(c.redirectUri, "code" -> Some(code), "state" -> state)
            case Some(_) => back(c.redirectUri, "error" -> Some("access_denied"), "state" -> state)
          }.map(_.removeCookie(ResponseCookie(CsrfCookie, "", path = Some("/oauth"))))
      }
    }

  private def postOauthToken(req: Request[IO]): IO[Response[IO]] =
    basicClient(req) match {
      case None => oauthError(Status.Unauthorized, "invalid_client", "クライアントの認証に失敗しました")
      case Some(c) =>
        form(req) { f =>
          val ip = Some(ReqInfo.clientIp(req))
          f.get("grant_type") match {
            case Some("authorization_code") =>
              (f.get("code"), f.get("redirect_uri"), f.get("code_verifier")) match {
                case (Some(code), Some(uri), Some(verifier)) if verifier.length >= 43 && verifier.length <= 128 =>
                  oauth.consumeCode(Crypto.sha256(code)).flatMap {
                    case Some(oc)
                        if oc.clientId == c.id && oc.redirectUri == uri &&
                          Crypto.constantTimeEquals(oc.codeChallenge, Crypto.pkceChallenge(verifier)) =>
                      val sc =
                        OAuthClient.Scopes.filter(x => oc.scope.split(' ').contains(x) && c.scopes.contains(x))
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
                          ip,
                          Json.obj("scopes" -> sc.asJson)
                        ) *>
                        tokenResponse(key, refresh, sc)
                    case _ => oauthError(Status.BadRequest, "invalid_grant", "認可コードが無効です（期限切れ・使用済み・不一致）")
                  }
                case _ => oauthError(
                    Status.BadRequest,
                    "invalid_request",
                    "code・redirect_uri・code_verifier（43〜128文字）を指定してください"
                  )
              }
            case Some("refresh_token") =>
              f.get("refresh_token").filter(_.nonEmpty) match {
                case None => oauthError(Status.BadRequest, "invalid_request", "refresh_token を指定してください")
                case Some(rt) =>
                  val (key, prefix, refresh) = newTokens()
                  oauth.refresh(
                    c.id,
                    Crypto.sha256(rt),
                    Crypto.sha256(refresh),
                    keyName(c),
                    prefix,
                    Crypto.sha256(key),
                    Instant.now().plusSeconds(TokenTtlSeconds)
                  )(_.filter(c.scopes.contains)).flatMap {
                    case Some(g) => tokenResponse(key, refresh, g.scopes)
                    case None => oauthError(Status.BadRequest, "invalid_grant", "refresh_token が無効です（取り消し済み・使用済み）")
                  }
              }
            case None => oauthError(Status.BadRequest, "invalid_request", "grant_type を指定してください")
            case Some(_) => oauthError(
                Status.BadRequest,
                "unsupported_grant_type",
                "grant_type は authorization_code か refresh_token です"
              )
          }
        }
    }

  private def postOauthRevoke(req: Request[IO]): IO[Response[IO]] =
    basicClient(req) match {
      case None => oauthError(Status.Unauthorized, "invalid_client", "クライアントの認証に失敗しました")
      case Some(c) => form(req)(f =>
          f.get("token").filter(_.nonEmpty).traverse_(t => oauth.revokeByToken(Crypto.sha256(t), c.id)) *>
            Ok(Json.obj())
        )
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

  private val CodeTtlSeconds = 60L
  private val TokenTtlSeconds = 3600L
  private val CsrfCookie = "skereru_oauth_csrf"

  private def client(id: String): Option[OAuthClient] = cfg.oauthClients.find(_.id == id)

  private def enc(s: String): String = java.net.URLEncoder.encode(s, "UTF-8")

  /** redirect_uri に結果を付けて戻す。 */
  private def back(uri: String, params: (String, Option[String])*): IO[Response[IO]] = {
    val q = params.collect { case (k, Some(v)) => s"$k=${enc(v)}" }.mkString("&")
    IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(uri +
      (if uri.contains("?") then "&" else "?") + q))))
  }

  // ---------------------------------------------------------------- 許可の画面（サーバーで HTML を組み立てる）

  /**
   * 許可の画面。CSP の form-action は付けない（フォームの送信後のリダイレクトの連鎖にも効くため、
   * 連携元が callback の後に別の origin へ戻すと止まる）。CSRF は Cookie と hidden の値の照合で防ぐ。
   */
  private def page(title: String, body: String): IO[Response[IO]] = {
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
    IO.pure(Response[IO](Status.Ok).withEntity(html)(using EntityEncoder.stringEncoder).putHeaders(
      `Content-Type`(MediaType.text.html, Charset.`UTF-8`),
      "Cache-Control" -> "no-store",
      "X-Frame-Options" -> "DENY",
      "Content-Security-Policy" -> "default-src 'none'; style-src 'unsafe-inline'; frame-ancestors 'none'"
    ))
  }

  private def errorPage(msg: String): IO[Response[IO]] =
    page(
      "認可できません",
      s"<h1>認可できません</h1><p>${Html.escape(msg)}</p><p class=\"muted\">連携元のサービスの設定を確認してください。</p>"
    ).map(_.withStatus(Status.BadRequest))

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

  /** 求められたスコープ（なければ read）。クライアントに許していないものが含まれていれば None。 */
  private def scopes(c: OAuthClient, requested: Option[String]): Option[List[String]] = {
    val rs = requested.map(
      _.split(' ').map(_.trim).filter(_.nonEmpty).toList.distinct
    ).filter(_.nonEmpty).getOrElse(List(ApiScope.Read))
    Option.when(rs.forall(c.scopes.contains))(OAuthClient.Scopes.filter(rs.contains))
  }

  /** 認可の要求の確認（client_id・redirect_uri が不正ならリダイレクトしない）。 */
  private def validate(p: String => Option[String])
    : Either[IO[Response[IO]], (OAuthClient, Option[String], String, List[String])] =
    (p("client_id").flatMap(client), p("redirect_uri")) match {
      case (None, _) => Left(errorPage("登録されていないクライアントです（client_id）"))
      case (Some(c), r) if !r.contains(c.redirectUri) => Left(errorPage("戻り先（redirect_uri）が登録されたものと一致しません"))
      case (Some(c), _) =>
        val state = p("state")
        if !p("response_type").contains("code") then
          Left(back(c.redirectUri, "error" -> Some("unsupported_response_type"), "state" -> state))
        else if !p("code_challenge_method").contains("S256") ||
          !p("code_challenge").exists(_.matches("^[A-Za-z0-9_-]{43}$"))
        then
          Left(back(
            c.redirectUri,
            "error" -> Some("invalid_request"),
            "error_description" -> Some("PKCE（S256）が必要です"),
            "state" -> state
          ))
        else
          scopes(c, p("scope")) match {
            case None => Left(back(
                c.redirectUri,
                "error" -> Some("invalid_scope"),
                "error_description" -> Some("このクライアントに許していないスコープです"),
                "state" -> state
              ))
            case Some(sc) => Right((c, state, p("code_challenge").get, sc))
          }
    }

  // ---------------------------------------------------------------- トークンの発行・失効（クライアント認証は Basic）

  private def oauthError(status: Status, code: String, desc: String): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(Json.obj("error" -> code.asJson, "error_description" -> desc.asJson))
      .putHeaders("Cache-Control" -> "no-store", "Pragma" -> "no-cache").putHeaders(
        (if status == Status.Unauthorized then List[Header.ToRaw]("WWW-Authenticate" -> "Basic realm=\"skereru\"")
         else Nil)*
      ))

  private def basicClient(req: Request[IO]): Option[OAuthClient] =
    req.headers.get[Authorization].collect { case Authorization(BasicCredentials(id, secret)) => (id, secret) }
      .flatMap((id, secret) => client(id).filter(c => Crypto.constantTimeEquals(c.secret, secret)))

  private def form(req: Request[IO])(f: Map[String, String] => IO[Response[IO]]): IO[Response[IO]] =
    req.as[UrlForm].attempt.flatMap {
      case Left(_) => oauthError(Status.BadRequest, "invalid_request", "application/x-www-form-urlencoded で送ってください")
      case Right(u) => f(u.values.view.mapValues(_.headOption.getOrElse("")).toMap)
    }

  /** 新しいアクセストークン（API キー）と refresh token。 */
  private def newTokens(): (String, String, String) = {
    val (key, prefix) = ApiKeys.generate()
    (key, prefix, "skrr_" + Crypto.randomToken(32))
  }

  private def tokenResponse(access: String, refresh: String, scopes: List[String]): IO[Response[IO]] =
    Ok(Json.obj(
      "access_token" -> access.asJson,
      "token_type" -> "Bearer".asJson,
      "expires_in" -> TokenTtlSeconds.asJson,
      "refresh_token" -> refresh.asJson,
      "scope" -> scopes.mkString(" ").asJson
    )).map(_.putHeaders("Cache-Control" -> "no-store", "Pragma" -> "no-cache"))

  private def keyName(c: OAuthClient) = s"${c.name} 連携"

}
