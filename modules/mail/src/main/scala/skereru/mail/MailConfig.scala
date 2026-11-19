package skereru.mail

import skereru.core.SpamThresholds

import java.net.InetAddress
import java.time.ZoneId

/** 外への送信を中継するサーバー（ポート25が使えない環境向け）。tls: starttls | ssl | none */
final case class RelayConfig(
  host: String,
  port: Int,
  tls: String,
  user: Option[String],
  password: Option[String])

/** メールのプロトコルと配送の設定。ポートが None ならその待ち受けをしない。 */
final case class MailConfig(
  hostname: String,
  bind: String = "0.0.0.0",
  smtpPort: Option[Int] = Some(25),
  submissionPort: Option[Int] = Some(587),
  smtpsPort: Option[Int] = Some(465),
  imapPort: Option[Int] = Some(143),
  imapsPort: Option[Int] = Some(993),
  pop3Port: Option[Int] = Some(110),
  pop3sPort: Option[Int] = Some(995),
  maxMessageBytes: Long = 50L * 1024 * 1024,
  maxRecipients: Int = 100,
  maxRecipientsPerHour: Int = 1000,
  defaultQuotaBytes: Long = 10L * 1024 * 1024 * 1024,
  thresholds: SpamThresholds = SpamThresholds(),
  dnsServers: List[String] = Nil,
  dnsblZones: List[String] = Nil,
  greylist: Boolean = false,
  greylistDelaySecs: Int = 60,
  clamav: Option[(String, Int)] = None,
  relay: Option[RelayConfig] = None,
  /** 平文の接続での認証を許す（開発用。本番では false）。 */
  allowPlainAuth: Boolean = false,
  /** PROXY プロトコルを受け付ける接続元（ロードバランサー）。 */
  proxyFrom: List[(InetAddress, Int)] = Nil,
  maxConnections: Int = 2000,
  maxConnectionsPerIp: Int = 30,
  /** 外へ送るときに相手の証明書を確かめる（false なら暗号化だけ）。 */
  outboundVerifyTls: Boolean = false,
  /** 外へ送るときに TLS を必須にする（相手が STARTTLS に対応していなければ送らない）。 */
  outboundRequireTls: Boolean = false,
  /** 外への送信で、内部のアドレス（プライベート・ループバック）の MX にも送る（試験環境用）。 */
  outboundAllowPrivate: Boolean = false,
  queueLifetimeHours: Int = 120,
  junkRetentionDays: Int = 30,
  trashRetentionDays: Int = 30,
  zone: ZoneId = ZoneId.of("Asia/Tokyo"),
  imapIdlePollMs: Int = 2000,
  version: String = "0.1.0")
