package skereru.mail

import cats.effect.*
import cats.syntax.all.*
import skereru.auth.AppPasswords
import skereru.db.*
import sbirds.observe.{Metrics, Telemetry}

import java.net.InetAddress
import java.time.Instant

/** メールソフト（IMAP・POP3・SMTP）のログイン。アプリパスワードで認証し、総当たりはロックする。 */
final class MailAuth(users: UserStore) {
  private val ipMax = 20 // 1つの IP から（どのアカウントでも）
  private val pairMax = 8 // 1つの IP から1つのアカウントへ

  private val windowSecs = 15 * 60
  private val lockSecs = 15 * 60

  def login(login: String, password: String, ip: InetAddress, proto: String): IO[Either[String, User]] = {
    // IPv6 は /64 単位で数える（/64 の中でアドレスを変えて、ロックを回避させない）
    val ipKey = s"ip:${IpKey.of(ip)}"
    val pairKey = s"pair:${IpKey.of(ip)}:${login.trim.toLowerCase}"
    val now = Instant.now()
    // アカウント全体ではロックしない（ほかの人が失敗を重ねて、正しい利用者を締め出せないように）。
    // アプリパスワードは約75ビットあるので、分散した総当たりでも現実的には当たらない
    (users.lockedUntil(ipKey, now), users.lockedUntil(pairKey, now)).tupled.flatMap {
      case (Some(_), _) | (_, Some(_)) =>
        Metrics.count("skereru.mail.logins", "proto" -> proto, "result" -> "locked") *>
          IO.sleep(scala.concurrent.duration.DurationInt(1).second).as(Left("認証の失敗が多いため、しばらくログインできません"))
      case _ =>
        if login.trim.isEmpty || password.isEmpty || login.length > 320 || password.length > 200 then
          IO.pure(Left("ユーザー名かパスワードが違います"))
        else
          users.checkAppPassword(login.trim, AppPasswords.hash(password), ip.getHostAddress).flatMap {
            case Some((u, _)) =>
              users.clearFailures(pairKey) *>
                Metrics.count("skereru.mail.logins", "proto" -> proto, "result" -> "ok") *>
                Telemetry.tagUser(u.id.toString, u.name).as(Right(u))
            case None =>
              for {
                l1 <- users.recordFailure(ipKey, ipMax, windowSecs, lockSecs)
                l3 <- users.recordFailure(pairKey, pairMax, windowSecs, lockSecs)
                _ <- users.audit(None, proto, "mail.login.failed", Some(login.take(200)), Some(ip.getHostAddress))
                _ <- IO.whenA(l1 || l3)(users.audit(
                  None,
                  proto,
                  "mail.login.locked",
                  Some(login.take(200)),
                  Some(ip.getHostAddress)
                ))
                _ <- Metrics.count("skereru.mail.logins", "proto" -> proto, "result" -> "failed")
                // 失敗の応答を遅らせて、総当たりを遅くする
                _ <- IO.sleep(scala.concurrent.duration.DurationInt(1).second)
              } yield Left("ユーザー名かパスワードが違います")
          }
    }
  }
}
