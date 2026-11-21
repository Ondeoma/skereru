package skereru.api

import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Authorization
import skereru.auth.SecretBox
import skereru.db.*
import skereru.mail.*
import skereru.storage.*

/**
 * API の試験の共通の環境（DB・REST・MCP・OAuth）。TEST_DATABASE_URL がなければ飛ばす（例: jdbc:postgresql://localhost:54370/skereru_test）。
 * oauthClients で OAuth のクライアントを登録する。
 */
trait ApiTestEnv extends CatsEffectSuite {

  protected def oauthClients: List[OAuthClient] = Nil

  private val dbCfg: Option[DbConfig] = sys.env.get("TEST_DATABASE_URL").map(url =>
    DbConfig(
      url,
      sys.env.getOrElse("TEST_DATABASE_USER", "skereru"),
      sys.env.getOrElse("TEST_DATABASE_PASSWORD", "skereru")
    )
  )

  /**
   * hooks: 通知先（Webhook）に送った要求（本文付き）. 外へは送らず、ここに貯める.
   * notifier: 受信の通知（送信待ちを送るのは試験から呼ぶ）.
   */
  final case class Env(app: HttpApp[IO],
                       users: UserStore,
                       hooks: java.util.concurrent.ConcurrentLinkedQueue[(Request[IO], String)],
                       notifier: NotifyService,
                       notifyStore: NotifyStore,
                       skawau: FakeSkawau,
                       skawauSvc: SkawauService,
                       tokens: sbirds.links.IntegrationTokenStore,
                       box: SecretBox)

  private val envFixture = ResourceSuiteLocalFixture(
    "env",
    dbCfg.fold(Resource.eval(IO.raiseError[Env](new RuntimeException("no db")))) { c =>
      for {
        _ <- Resource.eval(Database.migrate(c))
        xa <- Database.transactor(c)
        topic <- Resource.eval(fs2.concurrent.Topic[IO, java.util.UUID])
      } yield {
        val users = new UserStore(xa); val domains = new DomainStore(xa); val mail = new MailStore(xa);
        val queue = new QueueStore(xa)
        val schedules = new ScheduleStore(xa)
        val secret = "test-app-secret-0123456789"
        val mcfg = MailConfig(hostname = "mx.test.local", dnsServers = Nil)
        val cfg = ApiConfig(
          adminGroups = Set.empty,
          cookieSecure = false,
          publicUrl = "http://localhost",
          devLogin = true,
          oauthClients = oauthClients
        )
        val log = (_: String) => ()
        val box = SecretBox(secret)
        val dns = new JndiDns(Nil, 1000)
        val blobs = new PgBlobs(mail, new BlobCrypto(secret, true), new BlobCache(1024 * 1024))
        val changes = new ChangeHub(mail, topic)
        val delivery = new Delivery(
          mail,
          domains,
          users,
          queue,
          blobs,
          new SpamFilter(dns, queue, mcfg, log),
          new Signer(domains, box),
          mcfg,
          log,
          changes.publish
        )
        val sess = new SessionAuth(users, cfg)
        val hooks = new java.util.concurrent.ConcurrentLinkedQueue[(Request[IO], String)]()
        val hookHttp = org.http4s.client.Client.fromHttpApp(HttpApp[IO](r =>
          r.bodyText.compile.string.flatMap(b => IO(hooks.add(r -> b))).as(Response[IO](Status.NoContent))
        ))
        val notifyStore = new NotifyStore(xa)
        // 試験の送り先（hooks.example など）は名前を引けないので、宛先の確認はしない
        val notifier =
          new NotifyService(notifyStore, None, hookHttp, sbirds.net.NetGuard.Policy.allowAll, box, cfg.publicUrl)
        val adminSvc = new AdminService(xa, users, domains, mail, delivery, box, dns, mcfg, cfg)
        val mailSvc = new MailService(mail, users, domains, queue, blobs, delivery, mcfg, changes.publish, schedules)
        val fake = new FakeSkawau
        val tokens = new sbirds.links.IntegrationTokenStore(xa)
        val skawauLink = new sbirds.links.LinkClient(
          sbirds.links.LinkConfig(
            "skawau",
            FakeSkawau.Url,
            FakeSkawau.Url,
            "skereru",
            "skereru-secret-0123456789",
            "read write"
          ),
          org.http4s.client.Client.fromHttpApp(fake.app),
          tokens,
          box
        )
        val skawauSvc = new SkawauService(skawauLink, mailSvc, new SkawauSaveStore(xa), cfg.maxUploadBytes)
        val rest = new AuthRoutes(None, users, adminSvc, sess, cfg, mcfg).routes <+>
          new OAuthRoutes(new OAuthStore(xa), users, sess, cfg).routes <+>
          new MeRoutes(users, domains, mail, queue, sess, mcfg, notifyStore).routes <+>
          new SkawauRoutes(Some(skawauSvc), sess).routes <+>
          new LinkRoutes(Map("skawau" -> skawauLink), cfg.publicUrl, sess).routes <+>
          new NotifyRoutes(notifier, None, sess).routes <+>
          new MailRoutes(mailSvc, mail, schedules, sess, cfg, changes).routes <+>
          new AdminRoutes(adminSvc, users, domains, mail, queue, delivery, new DmarcStore(xa), sess).routes
        Env(
          (rest <+> new McpRoutes(rest, sess, cfg.publicUrl, cfg.version).routes).orNotFound,
          users,
          hooks,
          notifier,
          notifyStore,
          fake,
          skawauSvc,
          tokens,
          box
        )
      }
    }
  )

  override def munitFixtures = if dbCfg.isDefined then List(envFixture) else Nil

  protected def withEnv(name: String)(body: Env => IO[Unit]): Unit =
    test(name)(if dbCfg.isEmpty then IO(assume(false, "TEST_DATABASE_URL が未設定")) else body(envFixture()))

  /** 認証の手段（Cookie のセッションか APIキー）。 */
  protected enum Who {
    case Cookie(
      name: String,
      value: String)
    case Key(
      key: String)
  }

  protected def call(e: Env, who: Who, method: Method, path: String, body: Option[Json] = None): IO[(Status, Json)] = {
    val r0 = Request[IO](method, Uri.unsafeFromString(path))
    val r1 = who match {
      case Who.Cookie(n, v) => r0.addCookie(n, v).putHeaders("X-Skereru" -> "1")
      case Who.Key(k) => r0.putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, k)))
    }
    val req = body.fold(r1)(r1.withEntity(_))
    e.app.run(req).flatMap(res =>
      res.bodyText.compile.string.map(s => res.status -> io.circe.parser.parse(s).getOrElse(Json.Null))
    )
  }

  protected def devLogin(e: Env, name: String, admin: Boolean): IO[Who.Cookie] = {
    val req = Request[IO](Method.POST, Uri.unsafeFromString("/auth/dev-login")).putHeaders("X-Skereru" -> "1")
      .withEntity(Json.obj("name" -> name.asJson, "admin" -> admin.asJson))
    e.app.run(req).map { res =>
      val c = res.cookies.find(_.name == "skereru_session").getOrElse(fail("セッションの Cookie がありません"))
      Who.Cookie(c.name, c.content)
    }
  }

  /** MCP の tools/list のツールの名前。 */
  protected def mcpTools(e: Env, who: Who): IO[List[String]] =
    call(
      e,
      who,
      Method.POST,
      "/mcp",
      Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> "tools/list".asJson))
    ).map {
      (st, j) =>
        {
          assertEquals(st, Status.Ok, j.noSpaces)
          j.hcursor.downField(
            "result"
          ).downField("tools").as[List[Json]].toOption.getOrElse(Nil).flatMap(_.hcursor.get[String]("name").toOption)
        }
    }
}
