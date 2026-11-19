package skereru.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import sbirds.http.Errors
import sbirds.observe.Log
import skereru.core.ErrorCode
import org.http4s.server.staticcontent.*
import skereru.api.*
import skereru.auth.{OidcClient, OidcConfig, SecretBox}
import skereru.db.*
import skereru.mail.*
import sbirds.observe.{HttpClientTelemetry, HttpTelemetry, Metrics, RumRoutes, Telemetry}
import skereru.storage.*
import sbirds.storage.S3Config

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
  skuakaUrl: Option[String] = None)

object Main extends IOApp.Simple {

  // ログにトレースの ID・操作したユーザーを載せるため、IOLocal の文脈を Java のライブラリから見えるようにする
  // （IORuntime を作る前に設定する必要がある. オブジェクトの初期化は main より先に動く）
  Telemetry.prepare()

  private val version =
    Option(getClass.getPackage.getImplementationVersion).orElse(sys.env.get("APP_VERSION")).getOrElse("dev")

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  // メールのプロトコル（スレッドで動く処理）のログ. 標準出力と skorimako に、トレースの ID 付きで出る
  private val log: String => Unit = Log.line

  def run: IO[Unit] =
    for {
      cfg <- Config.load
      n <- migrateWithRetry(cfg)
      _ <- Log.println(s"[skereru] database migrated ($n migration(s) applied); node ${cfg.nodeId}")
      _ <- (for {
        _ <- Telemetry.resource("skereru", version, runtime)
        xa <- Database.transactor(cfg.db)
        _ <- gauges(xa, cfg)
        // 外への呼び出しもトレースに出す（traceparent を付けて送る）
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build.map(HttpClientTelemetry(_))
        users = new UserStore(xa)
        domains = new DomainStore(xa)
        mail = new MailStore(xa)
        queue = new QueueStore(xa)
        schedules = new ScheduleStore(xa)
        lock = new JobLock(xa, cfg.nodeId)
        box = SecretBox(cfg.appSecret)
        crypto = new BlobCrypto(cfg.appSecret, cfg.encryptAtRest)
        cache = new BlobCache(128L * 1024 * 1024)
        blobs <- cfg.s3 match {
          case Some(c) => S3Blobs.resource(c, mail, crypto, cache).widen[Blobs]
          case None => Resource.pure[IO, Blobs](new PgBlobs(mail, crypto, cache))
        }
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        tls <- Resource.eval(IO.blocking(new TlsHolder(
          cfg.tlsCert.map(Paths.get(_)),
          cfg.tlsKey.map(Paths.get(_)),
          cfg.mail.hostname,
          log
        )))
        topic <- Resource.eval(fs2.concurrent.Topic[IO, java.util.UUID])
        dns = new JndiDns(cfg.mail.dnsServers)
        changes = new ChangeHub(mail, topic)
        spam = new SpamFilter(dns, queue, cfg.mail, log)
        delivery = new Delivery(
          mail,
          domains,
          users,
          queue,
          blobs,
          spam,
          new Signer(domains, box, Some(dns)),
          cfg.mail,
          log,
          changes.publish
        )
        mailAuth = new MailAuth(users)
        sess = new SessionAuth(users, cfg.api)
        adminSvc = new AdminService(xa, users, domains, mail, delivery, box, dns, cfg.mail, cfg.api)
        mailSvc = new MailService(mail, users, domains, queue, blobs, delivery, cfg.mail, changes.publish, schedules)
        imports = new ImportStore(xa)
        dmarc = new DmarcStore(xa)
        importer = new Importer(mail, imports, blobs, box, dns, cfg.mail, log, changes.publish)
        invites = new InviteService(mailSvc, users, domains, mail, delivery, box, http, cfg.skuakaUrl, cfg.mail)
        worker = new OutboundWorker(queue, blobs, new SmtpClient(cfg.mail, dns), delivery, cfg.mail, log)
        pushStore = new sbirds.push.PushStore(xa)
        push <- Resource.eval(sbirds.push.PushService.create(pushStore, http, cfg.api.publicUrl, "skereru"))
        _ <- Resource.eval(delivery.refreshDomains *> startupNotes(cfg))
        // DMARC の集計レポートは、このサーバーのドメインについてのものだけを保存する
        _ = delivery.onDmarcReport = (
          raw,
          authenticated
        ) =>
          skereru.core.DmarcReport.fromMessage(raw) match {
            case Left(e) => Log.report(ErrorCode.DmarcReportUnreadable, e.toString)
            case Right(a) => delivery.isLocalDomain(a.domain).ifM(
                dmarc.save(a, authenticated),
                IO(log(s"[dmarc] report for unknown domain ${a.domain}"))
              )
          }
        // 受信トレイに届いた新着は、この端末で受け取る設定（PWA のプッシュ通知）にしていれば、ブラウザを閉じていても知らせる
        _ = delivery.onInboxMail = (
          uid,
          id,
          root
        ) => push.send(uid, newMailPush(id, root))
        _ <- MailServers.start(MailServers.Deps(cfg.mail, Some(tls), delivery, mailAuth, mail, blobs, queue, dns, log))
        loops = List[IO[Unit]](
          worker.loop,
          worker.expireLoop,
          // Gmail などからの IMAP の取り込み（待っているものを、どれかのノードが引き受ける）
          importer.loop(cfg.nodeId),
          // 予約送信（時刻が来たもの）とスヌーズ（時刻が来たもの）。行の取り合いは SKIP LOCKED で、どのノードでも動かしてよい
          Periodic.run(10.seconds, 2.seconds, ErrorCode.ScheduledSendFailed)(mailSvc.sendDue *> mailSvc.wakeSnoozed),
          Periodic.run(1.minute, 1.minute, ErrorCode.DomainRefreshFailed)(delivery.refreshDomains),
          Periodic.run(10.minutes, 10.minutes, ErrorCode.TlsReloadFailed)(IO.blocking(tls.reloadIfChanged())),
          Periodic.run(5.minutes, 1.hour, ErrorCode.PurgeFailed)(lock.exclusive("purge", 50.minutes)(
            IO.realTimeInstant.flatMap(users.purgeExpired) *> mail.purgeUploads() *> blobs.purge().void *>
              queue.purgeGreylist() *>
              queue.purgeLog(90) *> queue.purgeDone(30) *> dmarc.purge(400) *> users.purgeAudit(365) *>
              queue.purgeBayes().void
          )),
          Periodic.run(15.minutes, 6.hours, ErrorCode.RetentionFailed)(lock.exclusive("retention", 5.hours)(
            mail.purgeOld(skereru.shared.SpecialUse.Junk, cfg.mail.junkRetentionDays) *>
              mail.purgeOld(skereru.shared.SpecialUse.Trash, cfg.mail.trashRetentionDays).void
          ))
        )
        _ <- loops.traverse_(_.background)
        rest =
          healthRoutes(xa, cfg.nodeId) <+>
            new AuthRoutes(oidc, users, adminSvc, sess, cfg.api, cfg.mail).routes <+>
            new OAuthRoutes(new OAuthStore(xa), users, sess, cfg.api).routes <+>
            new MeRoutes(users, domains, mail, queue, sess, cfg.mail).routes <+>
            new AvatarRoutes(new AvatarStore(xa), sess).routes <+>
            new MailRoutes(mailSvc, mail, schedules, sess, cfg.api, changes).routes <+>
            new ImportRoutes(importer, imports, mail, blobs, sess).routes <+>
            new InviteRoutes(invites, sess).routes <+>
            new AdminRoutes(adminSvc, users, domains, mail, queue, delivery, dmarc, sess).routes <+>
            new WellKnownRoutes(domains, cfg.mail, cfg.stsMode).routes <+>
            RumRoutes.routes <+>
            new PushRoutes(pushStore, push, sess).routes
        app = rest <+> new McpRoutes(rest, sess, cfg.api.publicUrl, cfg.api.version).routes
        all = cfg.staticDir.fold(app)(d => app <+> spa(d))
        httpApp <- HttpTelemetry(
          SecurityHeaders(cfg.api)(org.http4s.server.middleware.EntityLimiter.httpApp(
            ReqInfo.middleware(cfg.trustedProxies)(Errors.handle(all.orNotFound, ErrorCode.Unexpected)),
            cfg.api.maxUploadBytes + 1024 * 1024
          )),
          List("/api", "/auth", "/mcp", "/.well-known")
        )
        _ <- EmberServerBuilder.default[IO]
          .withHost(Host.fromString(cfg.httpBind).getOrElse(ipv4"0.0.0.0"))
          .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
          .withShutdownTimeout(5.seconds)
          .withMaxHeaderSize(32 * 1024)
          // 本文の大きさの上限（添付ファイルの上限 + 1MB）。巨大な本文でメモリを使い切らせない
          .withHttpApp(httpApp)
          .build
        _ <- Resource.eval(Log.println(
          s"[skereru] listening on :${cfg.port} (${cfg.api.publicUrl}), mail host ${cfg.mail.hostname}"
        ))
      } yield ()).useForever
    } yield ()

  /** DB（Patroni のプライマリ）が使えるまで待ってからマイグレーションする。複数ノード同時起動でも Flyway のロックで1回だけ適用される。 */
  private def migrateWithRetry(
    cfg: AppConfig,
    attempt: Int = 1
  ): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else
        Log.report(ErrorCode.DatabaseNotReady, s"${e.getMessage.take(120)}; retry $attempt/60") *>
          IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  /** DB から定期的に読むメトリクス（skorimako のダッシュボード・アラート用）。 */
  private def gauges(
    xa: doobie.Transactor[IO],
    cfg: AppConfig
  ): Resource[IO, Unit] = {
    def q(
      sqlText: doobie.Fragment
    ): IO[Long] = sqlText.query[Long].unique.transact(xa)
    val quota = cfg.mail.defaultQuotaBytes
    List(
      Metrics.gauge("skereru.users.total", "有効なユーザーの数")(q(sql"select count(*) from app_user where not disabled")),
      Metrics.gauge(
        "skereru.users.active_24h",
        "24時間以内に画面にログインしたユーザーの数"
      )(q(sql"select count(*) from app_user where last_login_at > now() - interval '24 hours'")),
      Metrics.gauge(
        "skereru.sessions.active",
        "有効なセッションの数"
      )(q(sql"select count(*) from session where expires_at > now()")),
      Metrics.gauge("skereru.domains.total", "有効なドメインの数")(q(sql"select count(*) from domain where enabled")),
      Metrics.gauge(
        "skereru.queue.outbound",
        "外への送信待ちの数（配送のキューの長さ）"
      )(q(sql"select count(*) from outbound where status = 'queued'")),
      Metrics.gauge(
        "skereru.queue.outbound_retrying",
        "送信待ちのうち、再送中のものの数"
      )(q(sql"select count(*) from outbound where status = 'queued' and attempts > 0")),
      Metrics.gauge(
        "skereru.scheduled.pending",
        "予約送信の待ちの数"
      )(q(sql"select count(*) from scheduled_send where status = 'queued'")),
      Metrics.gauge(
        "skereru.imports.active",
        "実行中・待ちの取り込みの数"
      )(q(sql"select count(*) from import_job where status in ('queued', 'running')")),
      Metrics.gauge(
        "skereru.mailbox.used_bytes",
        "メールボックスの使用量の合計（バイト）"
      )(q(sql"select coalesce(sum(used_bytes), 0)::bigint from app_user")),
      Metrics.gauge("skereru.mailbox.near_quota", "容量の90%以上を使っているユーザーの数")(
        q(sql"select count(*) from app_user where not disabled and used_bytes >= 0.9 * coalesce(quota_bytes, $quota)")
      ),
      Metrics.gauge(
        "skereru.mail.received_24h",
        "24時間以内に外から受け取ったメールの数"
      )(q(sql"select count(*) from mail_log where direction = 'in' and at > now() - interval '24 hours'"))
    ).sequence_
  }

  /** 新着のプッシュ通知の中身（差出人・件名. 開くとそのメール）。画面の新着の通知（Notify）と同じ tag にして、重ねて出さない。 */
  private def newMailPush(
    id: java.util.UUID,
    root: skereru.core.MimePart
  ): sbirds.push.PushMessage = {
    val from = root.header("From").flatMap(skereru.core.Address.parseOne).map(a =>
      a.nameDecoded.getOrElse(a.address)
    ).getOrElse("新しいメール")
    val subject =
      root.header("Subject").map(skereru.core.Mime.decodeHeader).map(_.trim).filter(_.nonEmpty).getOrElse("(件名なし)")
    sbirds.push.PushMessage(from, subject, s"/m/$id", Some("skereru-new"))
  }

  /** ロードバランサー用のヘルスチェック。DB（プライマリ）に書けない状態なら 503。 */
  private def healthRoutes(
    xa: doobie.Transactor[IO],
    node: String
  ): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select not pg_is_in_recovery()".query[Boolean].unique.transact(xa).timeout(3.seconds).attempt.flatMap {
        case Right(true) => text(Status.Ok, s"ok $node")
        case Right(false) => text(Status.ServiceUnavailable, "database is read-only (not primary)")
        case Left(e) => Log.report(ErrorCode.HealthCheckFailed, "", e) *>
            text(Status.ServiceUnavailable, "database unavailable")
      }
  }

  private def text(
    status: Status,
    body: String
  ): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder))

  private def startupNotes(
    cfg: AppConfig
  ): IO[Unit] =
    IO.whenA(
      cfg.oidc.isEmpty
    )(Log.println("[skereru] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")) *>
      IO.whenA(
        cfg.mail.allowPlainAuth
      )(Log.println("[skereru] WARNING: ALLOW_PLAIN_AUTH=true のため、暗号化されていない接続でもパスワードを受け付けます（開発用）")) *>
      IO.whenA(
        !cfg.encryptAtRest
      )(Log.println("[skereru] WARNING: ENCRYPT_AT_REST=false のため、メッセージを暗号化せずに保存します")) *>
      IO.whenA(
        cfg.mail.relay.isEmpty
      )(Log.println("[skereru] 外への送信は MX へ直接行います（ポート25の外向きの通信が必要です。使えない環境では RELAY_HOST を設定してください）"))

  /** ビルド済みの画面の配信。存在しないパスは index.html（SPA）。 */
  private def spa(
    dir: String
  ): HttpRoutes[IO] = {
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> _
          if !Set("api", "auth", "mcp", "oauth", ".well-known").exists(p =>
            req.pathInfo.renderString.startsWith("/" + p)
          ) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(Errors.respond(ErrorCode.PageNotFound))
    }
    HttpRoutes[IO] { req =>
      val cc =
        if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc))
    }
  }
}
