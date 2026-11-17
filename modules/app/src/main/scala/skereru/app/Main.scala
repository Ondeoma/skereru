package skereru.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling
import org.http4s.server.staticcontent.*
import org.typelevel.ci.CIString
import skereru.api.*
import skereru.auth.{OidcClient, OidcConfig, SecretBox}
import skereru.core.SpamThresholds
import skereru.db.*
import skereru.mail.*
import skereru.shared.ErrorDto
import skereru.storage.*

import java.net.InetAddress
import java.nio.file.Paths
import scala.concurrent.duration.*

final case class AppConfig(
    port: Int,
    /** HTTP の待ち受けアドレス。 */
    httpBind: String,
    /** X-Forwarded-For を信じる接続元（ロードバランサー）。 */
    trustedProxies: List[(InetAddress, Int)],
    db: DbConfig,
    oidc: Option[OidcConfig],
    api: ApiConfig,
    mail: MailConfig,
    staticDir: Option[String],
    s3: Option[S3Config],
    appSecret: String,
    encryptAtRest: Boolean,
    tlsCert: Option[String],
    tlsKey: Option[String],
    stsMode: String,
    nodeId: String,
    /** 兄弟のカレンダー（skuaka）の URL。招待をカレンダーに足すときに使う（管理者が決める）。 */
    skuakaUrl: Option[String] = None
)

/** 設定は環境変数から読む（一覧は README）。 */
object Config:
  private def opt(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)
  private def int(name: String): Option[Int]    = opt(name).flatMap(_.toIntOption)
  private def bool(name: String, default: Boolean): Boolean = opt(name).map(_.equalsIgnoreCase("true")).getOrElse(default)
  private def required(name: String): Either[String, String] = opt(name).toRight(s"環境変数 $name を設定してください")
  /** ポート（"off" で待ち受けない）。 */
  private def port(name: String, default: Int): Option[Int] = opt(name) match
    case Some(v) if v.equalsIgnoreCase("off") => None
    case Some(v) => v.toIntOption.orElse(Some(default))
    case None    => Some(default)
  private def list(name: String): List[String] = opt(name).toList.flatMap(_.split("[,\\s]+").map(_.trim).filter(_.nonEmpty))
  private def cidrs(name: String): Either[String, List[(InetAddress, Int)]] =
    list(name).traverse(c => skereru.core.Cidr.parse(c).toRight(s"$name の $c が正しくありません"))
  /** テンプレートの値（change-me）のままの秘密は、本番では受け付けない。 */
  private def notPlaceholder(name: String, v: Option[String], devLogin: Boolean): Either[String, Unit] =
    Either.cond(devLogin || !v.exists(_.toLowerCase.contains("change-me")), (), s"$name がテンプレートの値（change-me）のままです。秘密の値に変えてください")

  def load: IO[AppConfig] = IO.fromEither(
    (for
      dbUrl  <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub    <- required("PUBLIC_URL")
      devLogin = bool("DEV_LOGIN", false)
      _ <- List("APP_SECRET", "DATABASE_PASSWORD", "OIDC_CLIENT_SECRET", "S3_SECRET_KEY", "RELAY_PASSWORD").traverse_(n => notPlaceholder(n, opt(n), devLogin))
      _ <- Either.cond(!(devLogin && pub.toLowerCase.startsWith("https://")), (), "DEV_LOGIN は開発用です。https の PUBLIC_URL（本番）では使えません")
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match
                case (Some(i), Some(c), Some(s)) =>
                  Right(Some(OidcConfig(i, c, s, s"${pub.stripSuffix("/")}/auth/callback", opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
                    trustEmail = bool("OIDC_TRUST_EMAIL", false))))
                case (None, None, None) if devLogin => Right(None)
                case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      secret <- opt("APP_SECRET") match
                  case Some(s) if s.length >= 16 => Right(s)
                  case Some(_) => Left("APP_SECRET は16文字以上にしてください")
                  case None if devLogin => Right("skereru-development-secret")
                  case None => Left("APP_SECRET（16文字以上の秘密の値。全ノードで同じ値）を設定してください")
      proxies <- cidrs("PROXY_PROTOCOL_FROM")
      trusted <- cidrs("TRUSTED_PROXIES").map(l => if opt("TRUSTED_PROXIES").isDefined then l
                   else List("127.0.0.0/8", "::1/128").flatMap(skereru.core.Cidr.parse))
      relay = opt("RELAY_HOST").map(h => RelayConfig(h, int("RELAY_PORT").getOrElse(587), opt("RELAY_TLS").getOrElse("starttls").toLowerCase,
                opt("RELAY_USER"), opt("RELAY_PASSWORD")))
      clamav <- opt("CLAMAV").traverse(v => v.split(':') match
                  case Array(h, p) if p.toIntOption.isDefined => Right((h, p.toInt))
                  case _ => Left("CLAMAV は host:port で指定してください"))
      skuaka <- opt("SKUAKA_URL").traverse(u => Either.cond(u.startsWith("https://") || (devLogin && u.startsWith("http://")), u.stripSuffix("/"),
                  "SKUAKA_URL は https:// で始めてください"))
      sts = opt("MTA_STS_MODE").getOrElse("testing").toLowerCase
      _ <- Either.cond(Set("testing", "enforce", "none")(sts), (), "MTA_STS_MODE は testing・enforce・none のどれかです")
    yield
      val base = pub.stripSuffix("/")
      val host = opt("MAIL_HOSTNAME").getOrElse(scala.util.Try(java.net.URI.create(base).getHost).toOption.flatMap(Option(_)).getOrElse("localhost"))
      val tz = opt("TIMEZONE").getOrElse("Asia/Tokyo")
      AppConfig(
        port = int("HTTP_PORT").getOrElse(8080),
        // 開発用のログインは誰でも管理者になれるので、既定ではこの機械の中からだけ受ける
        httpBind = opt("HTTP_BIND").getOrElse(if devLogin then "127.0.0.1" else "0.0.0.0"),
        trustedProxies = trusted,
        db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(20)),
        oidc = oidc,
        api = ApiConfig(
          adminGroups = list("ADMIN_GROUPS").toSet,
          adminSessionTtl = int("ADMIN_SESSION_HOURS").getOrElse(8).max(1).hours,
          cookieSecure = base.startsWith("https://"),
          publicUrl = base,
          devLogin = devLogin,
          maxUploadBytes = int("MAX_UPLOAD_MB").getOrElse(25).toLong * 1024 * 1024,
          timezone = tz,
          autoCreateUsers = bool("AUTO_CREATE_USERS", true)
        ),
        mail = MailConfig(
          hostname = host,
          bind = opt("MAIL_BIND").getOrElse(if devLogin then "127.0.0.1" else "0.0.0.0"),
          smtpPort = port("SMTP_PORT", 25),
          submissionPort = port("SUBMISSION_PORT", 587),
          smtpsPort = port("SMTPS_PORT", 465),
          imapPort = port("IMAP_PORT", 143),
          imapsPort = port("IMAPS_PORT", 993),
          pop3Port = port("POP3_PORT", 110),
          pop3sPort = port("POP3S_PORT", 995),
          maxMessageBytes = int("MAX_MESSAGE_MB").getOrElse(50).toLong * 1024 * 1024,
          maxRecipients = int("MAX_RECIPIENTS").getOrElse(100),
          maxRecipientsPerHour = int("MAX_RECIPIENTS_PER_HOUR").getOrElse(1000),
          defaultQuotaBytes = int("DEFAULT_QUOTA_MB").getOrElse(10240).toLong * 1024 * 1024,
          thresholds = SpamThresholds(opt("SPAM_JUNK_SCORE").flatMap(_.toDoubleOption).getOrElse(5.0), opt("SPAM_REJECT_SCORE").flatMap(_.toDoubleOption).getOrElse(15.0)),
          dnsServers = list("DNS_SERVERS"),
          dnsblZones = list("DNSBL_ZONES"),
          greylist = bool("GREYLIST", false),
          greylistDelaySecs = int("GREYLIST_DELAY").getOrElse(60),
          clamav = clamav,
          relay = relay,
          allowPlainAuth = bool("ALLOW_PLAIN_AUTH", false),
          proxyFrom = proxies,
          maxConnections = int("MAX_CONNECTIONS").getOrElse(2000),
          maxConnectionsPerIp = int("MAX_CONNECTIONS_PER_IP").getOrElse(30),
          outboundVerifyTls = bool("OUTBOUND_VERIFY_TLS", false),
          outboundRequireTls = bool("OUTBOUND_REQUIRE_TLS", false),
          outboundAllowPrivate = bool("OUTBOUND_ALLOW_PRIVATE", false),
          queueLifetimeHours = int("QUEUE_LIFETIME_HOURS").getOrElse(120),
          junkRetentionDays = int("JUNK_RETENTION_DAYS").getOrElse(30),
          trashRetentionDays = int("TRASH_RETENTION_DAYS").getOrElse(30),
          zone = Op.zone(tz)
        ),
        staticDir = opt("STATIC_DIR"),
        s3 = opt("S3_ENDPOINT").map(ep => S3Config(ep, opt("S3_REGION").getOrElse("us-east-1"), opt("S3_BUCKET").getOrElse("skereru"),
          opt("S3_ACCESS_KEY").getOrElse(""), opt("S3_SECRET_KEY").getOrElse(""), !opt("S3_PATH_STYLE").contains("false"))),
        appSecret = secret,
        encryptAtRest = bool("ENCRYPT_AT_REST", true),
        tlsCert = opt("TLS_CERT"),
        tlsKey = opt("TLS_KEY"),
        stsMode = sts,
        skuakaUrl = skuaka,
        nodeId = opt("NODE_ID").getOrElse(InetAddress.getLocalHost.getHostName + "-" + java.util.UUID.randomUUID().toString.take(8))
      )
    ).left.map(new IllegalArgumentException(_))
  )

object Main extends IOApp.Simple:

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  private val log: String => Unit = s => System.out.println(s)

  def run: IO[Unit] =
    for
      cfg <- Config.load
      n   <- migrateWithRetry(cfg)
      _   <- IO.println(s"[skereru] database migrated ($n migration(s) applied); node ${cfg.nodeId}")
      _ <- (for
        xa   <- Database.transactor(cfg.db)
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build
        users   = new UserStore(xa)
        domains = new DomainStore(xa)
        mail    = new MailStore(xa)
        queue   = new QueueStore(xa)
        schedules = new ScheduleStore(xa)
        lock    = new JobLock(xa, cfg.nodeId)
        box     = new SecretBox(cfg.appSecret)
        crypto  = new BlobCrypto(cfg.appSecret, cfg.encryptAtRest)
        cache   = new BlobCache(128L * 1024 * 1024)
        blobs <- cfg.s3 match
                   case Some(c) => S3Blobs.resource(c, mail, crypto, cache).widen[Blobs]
                   case None    => Resource.pure[IO, Blobs](new PgBlobs(mail, crypto, cache))
        oidc  <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        tls   <- Resource.eval(IO.blocking(new TlsHolder(cfg.tlsCert.map(Paths.get(_)), cfg.tlsKey.map(Paths.get(_)), cfg.mail.hostname, log)))
        topic <- Resource.eval(fs2.concurrent.Topic[IO, java.util.UUID])
        dns      = new JndiDns(cfg.mail.dnsServers)
        changes  = new ChangeHub(mail, topic)
        spam     = new SpamFilter(dns, queue, cfg.mail, log)
        delivery = new Delivery(mail, domains, users, queue, blobs, spam, new Signer(domains, box, Some(dns)), cfg.mail, log, changes.publish)
        mailAuth = new MailAuth(users)
        sess     = new SessionAuth(users, cfg.api)
        adminSvc = new AdminService(xa, users, domains, mail, delivery, box, dns, cfg.mail, cfg.api)
        mailSvc  = new MailService(mail, users, domains, queue, blobs, delivery, cfg.mail, changes.publish, schedules)
        imports  = new ImportStore(xa)
        dmarc    = new DmarcStore(xa)
        importer = new Importer(mail, imports, blobs, box, dns, cfg.mail, log, changes.publish)
        invites  = new InviteService(mailSvc, users, domains, mail, delivery, box, http, cfg.skuakaUrl, cfg.mail)
        worker   = new OutboundWorker(queue, blobs, new SmtpClient(cfg.mail, dns), delivery, cfg.mail, log)
        _ <- Resource.eval(delivery.refreshDomains *> startupNotes(cfg))
        // DMARC の集計レポートは、このサーバーのドメインについてのものだけを保存する
        _ = delivery.onDmarcReport = (raw, authenticated) =>
              skereru.core.DmarcReport.fromMessage(raw) match
                case Left(e)  => IO(log(s"[dmarc] unreadable report: $e"))
                case Right(a) => delivery.isLocalDomain(a.domain).ifM(dmarc.save(a, authenticated), IO(log(s"[dmarc] report for unknown domain ${a.domain}")))
        _ <- MailServers.start(MailServers.Deps(cfg.mail, Some(tls), delivery, mailAuth, mail, blobs, queue, dns, log))
        loops = List[IO[Unit]](
          worker.loop, worker.expireLoop,
          // Gmail などからの IMAP の取り込み（待っているものを、どれかのノードが引き受ける）
          importer.loop(cfg.nodeId),
          // 予約送信（時刻が来たもの）とスヌーズ（時刻が来たもの）。行の取り合いは SKIP LOCKED で、どのノードでも動かしてよい
          Periodic.run(10.seconds, 2.seconds)(e => s"[skereru] scheduled: $e")(mailSvc.sendDue *> mailSvc.wakeSnoozed),
          Periodic.run(1.minute, 1.minute)(e => s"[skereru] refresh domains: $e")(delivery.refreshDomains),
          Periodic.run(10.minutes, 10.minutes)(e => s"[skereru] tls reload: $e")(IO.blocking(tls.reloadIfChanged())),
          Periodic.run(5.minutes, 1.hour)(e => s"[skereru] purge: $e")(lock.exclusive("purge", 50.minutes)(
            IO.realTimeInstant.flatMap(users.purgeExpired) *> mail.purgeUploads() *> blobs.purge().void *> queue.purgeGreylist() *>
              queue.purgeLog(90) *> queue.purgeDone(30) *> dmarc.purge(400) *> users.purgeAudit(365) *> queue.purgeBayes().void)),
          Periodic.run(15.minutes, 6.hours)(e => s"[skereru] retention: $e")(lock.exclusive("retention", 5.hours)(
            mail.purgeOld(skereru.shared.SpecialUse.Junk, cfg.mail.junkRetentionDays) *> mail.purgeOld(skereru.shared.SpecialUse.Trash, cfg.mail.trashRetentionDays).void))
        )
        _ <- loops.traverse_(_.background)
        rest =
          healthRoutes(xa, cfg.nodeId) <+>
            new AuthRoutes(oidc, users, adminSvc, sess, cfg.api, cfg.mail).routes <+>
            new MeRoutes(users, domains, mail, queue, sess, cfg.mail).routes <+>
            new MailRoutes(mailSvc, mail, schedules, sess, cfg.api, changes).routes <+>
            new ImportRoutes(importer, imports, mail, blobs, sess).routes <+>
            new InviteRoutes(invites, sess).routes <+>
            new AdminRoutes(adminSvc, users, domains, mail, queue, delivery, dmarc, sess).routes <+>
            new WellKnownRoutes(domains, cfg.mail, cfg.stsMode).routes
        app = rest <+> new McpRoutes(rest, sess, cfg.api.publicUrl, cfg.api.version).routes
        all = cfg.staticDir.fold(app)(d => app <+> spa(d))
        _ <- EmberServerBuilder.default[IO]
               .withHost(Host.fromString(cfg.httpBind).getOrElse(ipv4"0.0.0.0"))
               .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
               .withShutdownTimeout(5.seconds)
               .withMaxHeaderSize(32 * 1024)
               // 本文の大きさの上限（添付ファイルの上限 + 1MB）。巨大な本文でメモリを使い切らせない
               .withHttpApp(SecurityHeaders(cfg.api)(org.http4s.server.middleware.EntityLimiter.httpApp(
                 ReqInfo.middleware(cfg.trustedProxies)(ErrorHandling.Custom.recoverWith(all.orNotFound)(handler)), cfg.api.maxUploadBytes + 1024 * 1024)))
               .build
        _ <- Resource.eval(IO.println(s"[skereru] listening on :${cfg.port} (${cfg.api.publicUrl}), mail host ${cfg.mail.hostname}"))
      yield ()).useForever
    yield ()

  /** DB（Patroni のプライマリ）が使えるまで待ってからマイグレーションする。複数ノード同時起動でも Flyway のロックで1回だけ適用される。 */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else IO.println(s"[skereru] database not ready (${e.getMessage.take(120)}); retry $attempt/60") *> IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  /** ロードバランサー用のヘルスチェック。DB（プライマリ）に書けない状態なら 503。 */
  private def healthRoutes(xa: doobie.Transactor[IO], node: String): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select not pg_is_in_recovery()".query[Boolean].unique.transact(xa).timeout(3.seconds).attempt.flatMap {
        case Right(true)  => text(Status.Ok, s"ok $node")
        case Right(false) => text(Status.ServiceUnavailable, "database is read-only (not primary)")
        case Left(e)      => IO.println(s"[skereru] healthz: ${e.getMessage}") *> text(Status.ServiceUnavailable, "database unavailable")
      }
  }

  private def text(status: Status, body: String): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder))

  private def startupNotes(cfg: AppConfig): IO[Unit] =
    IO.whenA(cfg.oidc.isEmpty)(IO.println("[skereru] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")) *>
      IO.whenA(cfg.mail.allowPlainAuth)(IO.println("[skereru] WARNING: ALLOW_PLAIN_AUTH=true のため、暗号化されていない接続でもパスワードを受け付けます（開発用）")) *>
      IO.whenA(!cfg.encryptAtRest)(IO.println("[skereru] WARNING: ENCRYPT_AT_REST=false のため、メッセージを暗号化せずに保存します")) *>
      IO.whenA(cfg.mail.relay.isEmpty)(IO.println("[skereru] 外への送信は MX へ直接行います（ポート25の外向きの通信が必要です。使えない環境では RELAY_HOST を設定してください）"))

  private val handler: PartialFunction[Throwable, IO[Response[IO]]] = { case e =>
    IO.println(s"[skereru] unhandled error: $e") *> InternalServerError(ErrorDto("サーバーでエラーが発生しました"))
  }

  /** ビルド済みの画面の配信。存在しないパスは index.html（SPA）。 */
  private def spa(dir: String): HttpRoutes[IO] =
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> _ if !Set("api", "auth", "mcp", ".well-known").exists(p => req.pathInfo.renderString.startsWith("/" + p)) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(NotFound())
    }
    HttpRoutes[IO] { req =>
      val cc = if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc))
    }

/** すべての応答に付けるセキュリティのヘッダー（ルートが付けたものは上書きしない）。 */
object SecurityHeaders:
  private val csp =
    "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self' data: blob: https:; font-src 'self' data:; " +
      "connect-src 'self'; frame-src 'self'; frame-ancestors 'self'; object-src 'none'; base-uri 'self'; form-action 'self'"

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
