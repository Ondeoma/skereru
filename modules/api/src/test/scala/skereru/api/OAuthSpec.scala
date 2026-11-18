package skereru.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.{Authorization, Location}
import skereru.auth.Crypto
import skereru.shared.*

import java.util.UUID

/** OAuth の提供側（認可コード + PKCE・refresh token・スコープ・連携の取り消し）の試験。TEST_DATABASE_URL がなければ飛ばす（ApiTestEnv）。 */
class OAuthSpec extends ApiTestEnv:

  private val Redirect = "http://localhost:5199/api/connections/callback"
  private val Secret   = "skakapo-test-secret-0123456789"

  override protected def oauthClients: List[OAuthClient] = List(
    OAuthClient("skakapo", Secret, Redirect, "skakapo", List("read", "write", "send")),
    OAuthClient("reader", "reader-test-secret-0123456789", "http://localhost:5200/callback", "reader", List("read"))
  )

  private val verifier  = Crypto.randomToken(48)
  private val challenge = Crypto.pkceChallenge(verifier)

  private def authorizeUri(clientId: String = "skakapo", redirect: String = Redirect, scope: String = "read write send", state: String = "st-1"): String =
    val q = List("response_type" -> "code", "client_id" -> clientId, "redirect_uri" -> redirect, "scope" -> scope, "state" -> state,
      "code_challenge" -> challenge, "code_challenge_method" -> "S256")
    "/oauth/authorize?" + q.map((k, v) => s"$k=${java.net.URLEncoder.encode(v, "UTF-8")}").mkString("&")

  private def getAuthorize(e: Env, uri: String, who: Option[Who.Cookie]): IO[Response[IO]] =
    val r = Request[IO](Method.GET, Uri.unsafeFromString(uri))
    e.app.run(who.fold(r)(c => r.addCookie(c.name, c.value)))

  private def query(res: Response[IO]): Map[String, String] =
    res.headers.get[Location].map(_.uri.query.params).getOrElse(Map.empty)

  /** 許可の画面を開いて「許可する」を押し、認可コードを受け取る。画面の HTML も返す。 */
  private def allow(e: Env, user: Who.Cookie, scope: String): IO[(String, String)] =
    for
      page <- getAuthorize(e, authorizeUri(scope = scope), Some(user))
      _ = assertEquals(page.status, Status.Ok)
      html <- page.bodyText.compile.string
      csrf = page.cookies.find(_.name == "skereru_oauth_csrf").map(_.content).getOrElse(fail("CSRF の Cookie がありません"))
      _ = assert(html.contains(s"""name="csrf" value="$csrf""""), html)
      form = UrlForm("client_id" -> "skakapo", "redirect_uri" -> Redirect, "scope" -> scope, "state" -> "st-1", "code_challenge" -> challenge,
               "code_challenge_method" -> "S256", "response_type" -> "code", "csrf" -> csrf, "decision" -> "allow")
      res <- e.app.run(Request[IO](Method.POST, Uri.unsafeFromString("/oauth/authorize")).addCookie(user.name, user.value)
               .addCookie("skereru_oauth_csrf", csrf).withEntity(form))
      _ = assertEquals(res.status, Status.Found)
      q = query(res)
      _ = assert(res.headers.get[Location].exists(_.uri.renderString.startsWith(Redirect)), res.headers.toString)
      _ = assertEquals(q.get("state"), Some("st-1"))
    yield (q.getOrElse("code", fail(s"認可コードがありません: $q")), html)

  private def token(e: Env, form: UrlForm, secret: String = Secret): IO[(Status, Json)] =
    e.app.run(Request[IO](Method.POST, Uri.unsafeFromString("/oauth/token"))
      .putHeaders(Authorization(BasicCredentials("skakapo", secret))).withEntity(form))
      .flatMap(res => res.as[Json].map(res.status -> _))

  private def exchange(e: Env, code: String): IO[(Status, Json)] =
    token(e, UrlForm("grant_type" -> "authorization_code", "code" -> code, "redirect_uri" -> Redirect, "code_verifier" -> verifier))

  private def str(j: Json, k: String): String = j.hcursor.get[String](k).getOrElse(fail(s"$k がありません: ${j.noSpaces}"))

  private def sendMessage(e: Env, who: Who): IO[Json] =
    call(e, who, Method.POST, "/mcp", Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 2.asJson, "method" -> "tools/call".asJson,
      "params" -> Json.obj("name" -> "send_message".asJson, "arguments" -> Json.obj("to" -> List("x@example.com").asJson, "subject" -> "s".asJson, "body" -> "b".asJson))))).map(_._2)

  withEnv("認可の要求: 未ログインならログインへ、知らないクライアント・違う戻り先は 400、許していないスコープは invalid_scope") { e =>
    for
      r1 <- getAuthorize(e, authorizeUri(), None)
      _ = assertEquals(r1.status, Status.Found)
      _ = assert(r1.headers.get[Location].exists(_.uri.renderString.startsWith("/auth/login?returnTo=%2Foauth%2Fauthorize")), r1.headers.toString)
      r2 <- getAuthorize(e, authorizeUri(clientId = "unknown"), None)
      _ = assertEquals(r2.status, Status.BadRequest)
      r3 <- getAuthorize(e, authorizeUri(redirect = "http://evil.example/callback"), None)
      _ = assertEquals(r3.status, Status.BadRequest)
      // reader には read しか許していない
      r4 <- getAuthorize(e, authorizeUri(clientId = "reader", redirect = "http://localhost:5200/callback", scope = "read send"), None)
      _ = assertEquals(r4.status, Status.Found)
      _ = assertEquals(query(r4).get("error"), Some("invalid_scope"))
      _ = assert(r4.headers.get[Location].exists(_.uri.renderString.startsWith("http://localhost:5200/callback")))
      // PKCE がなければ invalid_request
      r5 <- getAuthorize(e, authorizeUri().replace("code_challenge_method=S256", "code_challenge_method=plain"), None)
      _ = assertEquals(query(r5).get("error"), Some("invalid_request"))
    yield ()
  }

  withEnv("許可 → コード → トークン → MCP・REST、refresh token、send のないトークンでは送信できない、連携の取り消し") { e =>
    val tag = UUID.randomUUID().toString.take(8)
    for
      user <- devLogin(e, s"oauth-$tag", admin = false)
      // send を含む許可。画面に「メールを送信できる」と出る
      a1 <- allow(e, user, "read write send")
      (code1, html1) = a1
      _ = assert(html1.contains("メールを送信できる"), html1)
      // クライアントの認証の失敗
      bad <- token(e, UrlForm("grant_type" -> "authorization_code", "code" -> code1, "redirect_uri" -> Redirect, "code_verifier" -> verifier), "wrong-secret-0123456789")
      _ = assertEquals(bad._1, Status.Unauthorized)
      t1 <- exchange(e, code1)
      _ = assertEquals(t1._1, Status.Ok, t1._2.noSpaces)
      access1 = str(t1._2, "access_token"); refresh1 = str(t1._2, "refresh_token")
      _ = assertEquals(str(t1._2, "scope"), "read write send")
      _ = assert(t1._2.hcursor.get[Long]("expires_in").exists(_ > 0))
      // コードは一度だけ
      again <- exchange(e, code1)
      _ = assertEquals((again._1, again._2.hcursor.get[String]("error").toOption), (Status.BadRequest, Some("invalid_grant")))
      // MCP の tools/list と REST
      tools1 <- mcpTools(e, Who.Key(access1))
      _ = assert(tools1.contains("send_message") && tools1.contains("search_messages"), tools1.toString)
      me <- call(e, Who.Key(access1), Method.GET, "/api/me")
      _ = assertEquals(me._1, Status.Ok)
      _ = assertEquals(me._2.hcursor.get[String]("name").toOption, Some(s"oauth-$tag"))
      // 自分の API キーの一覧には出ない
      keys <- call(e, user, Method.GET, "/api/me/api-keys")
      _ = assertEquals(keys._2.as[List[ApiKeyDto]].toOption.map(_.size), Some(0))
      // refresh token: 新しいトークンが使え、古い refresh token は使えない
      t2 <- token(e, UrlForm("grant_type" -> "refresh_token", "refresh_token" -> refresh1))
      _ = assertEquals(t2._1, Status.Ok, t2._2.noSpaces)
      access2 = str(t2._2, "access_token"); refresh2 = str(t2._2, "refresh_token")
      _ = assertNotEquals(refresh2, refresh1)
      me2 <- call(e, Who.Key(access2), Method.GET, "/api/me")
      _ = assertEquals(me2._1, Status.Ok)
      reuse <- token(e, UrlForm("grant_type" -> "refresh_token", "refresh_token" -> refresh1))
      _ = assertEquals(reuse._2.hcursor.get[String]("error").toOption, Some("invalid_grant"))
      // send を含まない許可に変える（前の許可のトークンは使えなくなる）
      a3 <- allow(e, user, "read write")
      (code3, html3) = a3
      _ = assert(!html3.contains("メールを送信できる"), html3)
      t3 <- exchange(e, code3)
      _ = assertEquals(t3._1, Status.Ok, t3._2.noSpaces)
      access3 = str(t3._2, "access_token")
      _ = assertEquals(str(t3._2, "scope"), "read write")
      old <- call(e, Who.Key(access2), Method.GET, "/api/me")
      _ = assertEquals(old._1, Status.Unauthorized)
      // send のないトークンでは、送信は REST・MCP（専用のツール・call_api）のどれでも拒まれる
      tools3 <- mcpTools(e, Who.Key(access3))
      _ = assert(!tools3.contains("send_message") && tools3.contains("update_messages"), tools3.toString)
      compose = Json.obj("from" -> "x@example.com".asJson, "to" -> List("y@example.com").asJson, "cc" -> Json.arr(), "bcc" -> Json.arr(),
                  "subject" -> "s".asJson, "text" -> "b".asJson, "html" -> Json.Null, "uploadIds" -> Json.arr(), "inReplyTo" -> Json.Null,
                  "forwardOf" -> Json.Null, "draftId" -> Json.Null)
      rs <- call(e, Who.Key(access3), Method.POST, "/api/send", Some(compose))
      _ = assertEquals(rs._1, Status.Forbidden, rs._2.noSpaces)
      sm <- sendMessage(e, Who.Key(access3))
      _ = assertEquals(sm.hcursor.downField("result").get[Boolean]("isError").toOption, Some(true), sm.noSpaces)
      ca <- call(e, Who.Key(access3), Method.POST, "/mcp", Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 3.asJson, "method" -> "tools/call".asJson,
              "params" -> Json.obj("name" -> "call_api".asJson, "arguments" -> Json.obj("method" -> "POST".asJson, "path" -> "/api/send".asJson, "body" -> compose)))))
      _ = assertEquals(ca._2.hcursor.downField("result").get[Boolean]("isError").toOption, Some(true), ca._2.noSpaces)
      // 書き込み（write）はできる
      w <- call(e, Who.Key(access3), Method.PATCH, "/api/me", Some(Json.obj("timezone" -> "UTC".asJson)))
      _ = assertEquals(w._1, Status.NoContent)
      // 連携の確認と取り消し（画面からのみ）
      gs <- call(e, user, Method.GET, "/api/me/oauth-grants")
      grants = gs._2.as[List[OAuthGrantDto]].fold(x => fail(x.getMessage), identity)
      _ = assertEquals(grants.map(g => (g.clientId, g.name, g.scopes)), List(("skakapo", "skakapo", List("read", "write"))))
      byKey <- call(e, Who.Key(access3), Method.GET, "/api/me/oauth-grants")
      _ = assertEquals(byKey._1, Status.Forbidden)
      del <- call(e, user, Method.DELETE, "/api/me/oauth-grants/skakapo")
      _ = assertEquals(del._1, Status.NoContent)
      gone <- call(e, Who.Key(access3), Method.GET, "/api/me")
      _ = assertEquals(gone._1, Status.Unauthorized)
      rf <- token(e, UrlForm("grant_type" -> "refresh_token", "refresh_token" -> str(t3._2, "refresh_token")))
      _ = assertEquals(rf._2.hcursor.get[String]("error").toOption, Some("invalid_grant"))
      del2 <- call(e, user, Method.DELETE, "/api/me/oauth-grants/skakapo")
      _ = assertEquals(del2._1, Status.NotFound)
    yield ()
  }
