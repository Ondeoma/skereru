package skereru.core

import java.net.InetAddress

/** DNS の問い合わせ（SPF・DKIM・DMARC・DNSBL・MX）。名前がない・レコードがないときは Right(Nil)、一時的な失敗は Left。 */
trait Dns {
  def txt(name: String): Either[String, List[String]]
  def addresses(name: String): Either[String, List[InetAddress]]
  def mx(name: String): Either[String, List[(Int, String)]]
  def ptr(ip: InetAddress): Either[String, List[String]]
}

enum SpfResult {
  case Pass, Fail, SoftFail, Neutral, None, TempError, PermError
  def label: String = this.toString.toLowerCase
}

/** ヘッダーの元の行（折り返しを含む。DKIM の simple の正規化に使う）。 */
final case class RawHeader(name: String, raw: String)

final case class DkimResult(domain: String,
                            selector: String,
                            result: String,
                            reason: String,
                            aligned: Boolean = false) {
  def pass: Boolean = result == "pass"
}

final case class DmarcResult(result: String, policy: String, domain: String) {
  def pass: Boolean = result == "pass"
}

/** 組織ドメイン（公開接尾辞の1つ下。Public Suffix List で判定する。tokyo.jp・co.il なども正しく扱う）。 */
object OrgDomain {
  def of(domain: String): String = {
    val d = domain.toLowerCase.stripSuffix(".")
    scala.util.Try {
      val n = com.google.common.net.InternetDomainName.from(d)
      if n.isUnderPublicSuffix then n.topPrivateDomain().toString
      else if n.isPublicSuffix then d
      else d.split('.').takeRight(2).mkString(".") // 一覧にない TLD（.example・.test など）
    }.getOrElse(d)
  }
}
