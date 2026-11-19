package skereru.app

import cats.effect.*
import cats.syntax.all.*
import skereru.api.*
import skereru.auth.OidcConfig
import skereru.core.SpamThresholds
import skereru.db.*
import skereru.mail.*
import sbirds.storage.S3Config

import java.net.InetAddress
import scala.concurrent.duration.*

/** 設定は環境変数から読む（一覧は README）。 */
object Config {
  private def opt(
    name: String
  ): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)
  private def int(
    name: String
  ): Option[Int] = opt(name).flatMap(_.toIntOption)
  private def bool(
    name: String,
    default: Boolean
  ): Boolean = opt(name).map(_.equalsIgnoreCase("true")).getOrElse(default)
  private def required(
    name: String
  ): Either[String, String] = opt(name).toRight(s"環境変数 $name を設定してください")

  /** ポート（"off" で待ち受けない）。 */
  private def port(
    name: String,
    default: Int
  ): Option[Int] = opt(name) match {
    case Some(v) if v.equalsIgnoreCase("off") => None
    case Some(v) => v.toIntOption.orElse(Some(default))
    case None => Some(default)
  }
  private def list(
    name: String
  ): List[String] = opt(name).toList.flatMap(_.split("[,\\s]+").map(_.trim).filter(_.nonEmpty))
  private def cidrs(
    name: String
  ): Either[String, List[(InetAddress, Int)]] =
    list(name).traverse(c => skereru.core.Cidr.parse(c).toRight(s"$name の $c が正しくありません"))

  /** テンプレートの値（change-me）のままの秘密は、本番では受け付けない。 */
  private def notPlaceholder(
    name: String,
    v: Option[String],
    devLogin: Boolean
  ): Either[String, Unit] =
    Either.cond(
      devLogin || !v.exists(_.toLowerCase.contains("change-me")),
      (),
      s"$name がテンプレートの値（change-me）のままです。秘密の値に変えてください"
    )

  /** OAUTH_CLIENTS: [{"id":"skakapo","secret":"…","redirectUri":"https://…/callback","name":"skakapo","scopes":["read","write","send"]}]（scopes は省くと read だけ） */
  private def oauthClients: Either[String, List[OAuthClient]] =
    opt("OAUTH_CLIENTS").fold(Right(Nil)) { s =>
      io.circe.parser.parse(s).flatMap(_.as[List[io.circe.Json]]).left.map(_ =>
        "OAUTH_CLIENTS は JSON の配列で指定してください"
      ).flatMap(_.traverse { j =>
        val c = j.hcursor
        (c.get[String]("id"), c.get[String]("secret"), c.get[String]("redirectUri")).tupled.toOption
          .filter(
            (
              id,
              sec,
              uri
            ) => id.nonEmpty && sec.length >= 16 && (uri.startsWith("https://") || uri.startsWith("http://localhost"))
          )
          .toRight("OAUTH_CLIENTS の各要素には id・secret（16文字以上）・redirectUri（https://）を指定してください")
          .flatMap(
            (
              id,
              sec,
              uri
            ) =>
              c.get[List[String]]("scopes").toOption.getOrElse(List("read")) match {
                case sc if sc.nonEmpty && sc.forall(OAuthClient.Scopes.contains) =>
                  Right(OAuthClient(id, sec, uri, c.get[String]("name").getOrElse(id), sc.distinct))
                case _ => Left(s"OAUTH_CLIENTS の $id の scopes は read・write・send から選んでください")
              }
          )
      }).flatMap(cs => Either.cond(cs.map(_.id).distinct.size == cs.size, cs, "OAUTH_CLIENTS の id が重なっています"))
    }

  def load: IO[AppConfig] = IO.fromEither(
    (for {
      dbUrl <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub <- required("PUBLIC_URL")
      devLogin = bool("DEV_LOGIN", false)
      _ <- List("APP_SECRET", "DATABASE_PASSWORD", "OIDC_CLIENT_SECRET", "S3_SECRET_KEY", "RELAY_PASSWORD").traverse_(
        n => notPlaceholder(n, opt(n), devLogin)
      )
      _ <- Either.cond(
        !(devLogin && pub.toLowerCase.startsWith("https://")),
        (),
        "DEV_LOGIN は開発用です。https の PUBLIC_URL（本番）では使えません"
      )
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match {
        case (Some(i), Some(c), Some(s)) =>
          Right(Some(OidcConfig(
            i,
            c,
            s,
            s"${pub.stripSuffix("/")}/auth/callback",
            opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
            trustEmail = bool("OIDC_TRUST_EMAIL", false)
          )))
        case (None, None, None) if devLogin => Right(None)
        case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      }
      secret <- opt("APP_SECRET") match {
        case Some(s) if s.length >= 16 => Right(s)
        case Some(_) => Left("APP_SECRET は16文字以上にしてください")
        case None if devLogin => Right("skereru-development-secret")
        case None => Left("APP_SECRET（16文字以上の秘密の値。全ノードで同じ値）を設定してください")
      }
      proxies <- cidrs("PROXY_PROTOCOL_FROM")
      trusted <- cidrs("TRUSTED_PROXIES").map(l =>
        if opt("TRUSTED_PROXIES").isDefined then l
        else List("127.0.0.0/8", "::1/128").flatMap(skereru.core.Cidr.parse)
      )
      relay = opt("RELAY_HOST").map(h =>
        RelayConfig(
          h,
          int("RELAY_PORT").getOrElse(587),
          opt("RELAY_TLS").getOrElse("starttls").toLowerCase,
          opt("RELAY_USER"),
          opt("RELAY_PASSWORD")
        )
      )
      clamav <- opt("CLAMAV").traverse(v =>
        v.split(':') match {
          case Array(h, p) if p.toIntOption.isDefined => Right((h, p.toInt))
          case _ => Left("CLAMAV は host:port で指定してください")
        }
      )
      skuaka <- opt("SKUAKA_URL").traverse(u =>
        Either.cond(
          u.startsWith("https://") || (devLogin && u.startsWith("http://")),
          u.stripSuffix("/"),
          "SKUAKA_URL は https:// で始めてください"
        )
      )
      clients <- oauthClients
      sts = opt("MTA_STS_MODE").getOrElse("testing").toLowerCase
      _ <- Either.cond(Set("testing", "enforce", "none")(sts), (), "MTA_STS_MODE は testing・enforce・none のどれかです")
    } yield {
      val base = pub.stripSuffix("/")
      val host = opt(
        "MAIL_HOSTNAME"
      ).getOrElse(scala.util.Try(java.net.URI.create(base).getHost).toOption.flatMap(Option(_)).getOrElse("localhost"))
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
          autoCreateUsers = bool("AUTO_CREATE_USERS", true),
          oauthClients = clients
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
          thresholds = SpamThresholds(
            opt("SPAM_JUNK_SCORE").flatMap(_.toDoubleOption).getOrElse(5.0),
            opt("SPAM_REJECT_SCORE").flatMap(_.toDoubleOption).getOrElse(15.0)
          ),
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
        s3 = opt("S3_ENDPOINT").map(ep =>
          S3Config(
            ep,
            opt("S3_REGION").getOrElse("us-east-1"),
            opt("S3_BUCKET").getOrElse("skereru"),
            opt("S3_ACCESS_KEY").getOrElse(""),
            opt("S3_SECRET_KEY").getOrElse(""),
            !opt("S3_PATH_STYLE").contains("false")
          )
        ),
        appSecret = secret,
        encryptAtRest = bool("ENCRYPT_AT_REST", true),
        tlsCert = opt("TLS_CERT"),
        tlsKey = opt("TLS_KEY"),
        stsMode = sts,
        skuakaUrl = skuaka,
        nodeId = opt("NODE_ID").getOrElse(InetAddress.getLocalHost.getHostName + "-" +
          java.util.UUID.randomUUID().toString.take(8))
      )
    }).left.map(new IllegalArgumentException(_))
  )
}
