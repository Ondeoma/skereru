package skereru.mail

import java.net.InetAddress
import java.util.UUID

/** 宛先の解決結果（配送するユーザーと、外への転送先）。 */
final case class Target(
  users: List[UUID],
  forwards: List[String],
  dmarcReports: Boolean = false)

/** 受信したメッセージ（エンベロープ）。 */
final case class Envelope(
  ip: InetAddress,
  helo: String,
  mailFrom: String,
  rcpts: List[String],
  tls: Boolean,
  authUser: Option[UUID])

enum InboundResult {
  case Accepted(
    id: String)
  case Rejected(
    code: String,
    msg: String)
  case TempFail(
    code: String,
    msg: String)
}
