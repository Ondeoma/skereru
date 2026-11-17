package skereru.api

import cats.effect.*
import doobie.Transactor
import doobie.implicits.*
import skereru.auth.SecretBox
import skereru.core.*
import skereru.db.*
import skereru.mail.{Delivery, JndiDns, MailConfig}
import skereru.shared.*

import java.util.UUID

/** ドメイン・ユーザー・アドレスの管理と、DNS に設定するレコードの案内。 */
final class AdminService(xa: Transactor[IO], users: UserStore, domains: DomainStore, mail: MailStore, delivery: Delivery, box: SecretBox, dns: JndiDns,
    mcfg: MailConfig, cfg: ApiConfig):

  private def newKey(): (String, String, String) =
    val (priv, pub) = Dkim.generate()
    val selector = "skr" + java.time.LocalDate.now(Op.zone(cfg.timezone)).toString.take(7).replace("-", "")
    (selector, box.encrypt(java.util.Base64.getEncoder.encodeToString(priv)), pub)

  /** ドメイン名を正規化する（IDN は punycode に、小文字に）。 */
  def normalizeDomain(name: String): Either[Fail, String] =
    val n = scala.util.Try(java.net.IDN.toASCII(name.trim.stripSuffix("."), java.net.IDN.ALLOW_UNASSIGNED)).getOrElse("").toLowerCase
    Either.cond(Address.validDomain(n), n, Fail.Invalid("ドメイン名が正しくありません"))

  def createDomain(r: CreateDomainReq): Res[UUID] =
    normalizeDomain(r.name) match
      case Left(f) => IO.pure(Left(f))
      case Right(n) =>
        val (sel, priv, pub) = newKey()
        domains.createDomain(n, r.autoProvision, sel, priv, pub).orFail.flatTap(_ => delivery.refreshDomains)

  def rotateDkim(id: UUID): IO[Int] =
    val (sel, priv, pub) = newKey()
    domains.rotateDkim(id, sel, priv, pub)

  /** DNS に設定するレコードと、今の設定が合っているか。 */
  def dnsRecords(id: UUID): Res[List[DnsRecordDto]] =
    domains.domain(id).flatMap {
      case None => IO.pure(Left(Fail.NotFound("ドメインが見つかりません")))
      case Some(d) =>
        IO.blocking {
          val host = mcfg.hostname
          val webHost = scala.util.Try(java.net.URI.create(cfg.publicUrl).getHost).toOption.getOrElse(host)
          def txt(n: String) = dns.txt(n).toOption.getOrElse(Nil)
          val mx = dns.mx(d.name).toOption.getOrElse(Nil)
          val spf = txt(d.name).find(_.toLowerCase.startsWith("v=spf1"))
          val dkim = txt(s"${d.dkimSelector}._domainkey.${d.name}").headOption
          val dmarc = txt(s"_dmarc.${d.name}").find(_.toLowerCase.startsWith("v=dmarc1"))
          val sts = txt(s"_mta-sts.${d.name}").headOption
          val rpt = txt(s"_smtp._tls.${d.name}").headOption
          val hostIps = dns.addresses(host).toOption.getOrElse(Nil)
          val ptr = hostIps.headOption.flatMap(ip => dns.ptr(ip).toOption.flatMap(_.headOption))
          val dkimValue = Dkim.dnsRecord(d.dkimPublic)
          List(
            DnsRecordDto("MX", d.name, s"10 $host", "メールの受け取り先", Some(mx.exists(_._2.equalsIgnoreCase(host))), Option.when(mx.nonEmpty)(mx.map((p, h) => s"$p $h").mkString(", "))),
            DnsRecordDto("TXT", d.name, s"v=spf1 mx a:$host -all", "SPF（送信を許すサーバー）", Some(spf.exists(_.toLowerCase.contains("mx")) || spf.exists(_.contains(host))), spf),
            DnsRecordDto("TXT", s"${d.dkimSelector}._domainkey.${d.name}", dkimValue, "DKIM（送信したメールの署名の公開鍵）",
              Some(dkim.exists(_.replace(" ", "").contains(d.dkimPublic))), dkim),
            DnsRecordDto("TXT", s"_dmarc.${d.name}", s"v=DMARC1; p=quarantine; adkim=s; aspf=r; rua=mailto:dmarc-reports@${d.name}", "DMARC（なりすましの扱い）",
              Some(dmarc.isDefined), dmarc),
            DnsRecordDto("TXT", s"_mta-sts.${d.name}", s"v=STSv1; id=${d.createdAt.getEpochSecond}", "MTA-STS（受信の TLS を必須にする）", Some(sts.isDefined), sts),
            DnsRecordDto("CNAME", s"mta-sts.${d.name}", webHost, "MTA-STS のポリシーの配信（このサーバーの Web）", None, None),
            DnsRecordDto("TXT", s"_smtp._tls.${d.name}", s"v=TLSRPTv1; rua=mailto:postmaster@${d.name}", "TLS-RPT（TLS の失敗の報告先）", Some(rpt.isDefined), rpt),
            DnsRecordDto("CNAME", s"autoconfig.${d.name}", webHost, "メールソフトの自動設定（Thunderbird など）", None, None),
            DnsRecordDto("PTR", hostIps.headOption.map(_.getHostAddress).getOrElse("(サーバーの IP)"), host, "逆引き（サーバーの IP の管理者に設定してもらう）",
              Some(ptr.exists(_.equalsIgnoreCase(host))), ptr)
          )
        }.map(Right(_))
    }

  /** メールのユーザーを作る（主アドレスつき）。 */
  def createUser(r: CreateUserReq): Res[UUID] =
    val addr = Address.toAscii(r.address.trim).toLowerCase
    if r.name.trim.isEmpty then IO.pure(Left(Fail.Invalid("名前を入力してください")))
    else if !Address.valid(addr) then IO.pure(Left(Fail.Invalid("メールアドレスが正しくありません")))
    else if r.email.exists(e => !Address.valid(Address.toAscii(e.trim))) then IO.pure(Left(Fail.Invalid("IdP のメールアドレスが正しくありません")))
    else
      domains.domainByName(Address.domainOf(addr)).flatMap {
        case None => IO.pure(Left(Fail.Invalid(s"ドメイン ${Address.domainOf(addr)} が登録されていません")))
        case Some(d) =>
          (for
            uid <- users.create(r.name.trim.take(100), r.email.map(_.trim.toLowerCase).orElse(Some(addr)), r.quotaMb.map(_.toLong * 1024 * 1024))
            a <- domains.createAddress(addr, d.id, Some(uid), Nil, true, true)
            _ <- a.fold(e => doobie.free.connection.raiseError(new IllegalStateException(e.message)), _ => doobie.free.connection.unit)
            _ <- mail.ensureDefaultsC(uid)
          yield uid).transact(xa).attempt.map(_.left.map(e => Fail.Conflict(Option(e.getMessage).getOrElse("作れませんでした"))))
      }

  /** IdP でログインした人のアドレスを自動で作る（ドメインが auto_provision のとき）。 */
  def autoProvision(u: User): IO[Unit] =
    u.email.map(e => Address.toAscii(e).toLowerCase).filter(Address.valid) match
      case None => IO.unit
      case Some(e) =>
        domains.addresses(None, Some(u.id)).flatMap { mine =>
          if mine.nonEmpty then IO.unit
          else domains.domainByName(Address.domainOf(e)).flatMap {
            case Some(d) if d.autoProvision =>
              domains.address(e).flatMap {
                case None    => domains.createAddressIO(e, d.id, Some(u.id), Nil, true, true).void
                case Some(_) => IO.unit
              }
            case _ => IO.unit
          }
        } *> mail.ensureDefaults(u.id)

  def adminUsers(q: Option[String]): IO[List[AdminUserDto]] =
    for
      us <- users.list(q, 500)
      addrs <- domains.addresses(None, None)
    yield us.map { u =>
      AdminUserDto(u.id, u.name, u.email, u.isAdmin, u.disabled, u.oidcSub.isDefined, addrs.filter(_.userId.contains(u.id)).map(_.address),
        u.usedBytes, u.quotaBytes.getOrElse(mcfg.defaultQuotaBytes), u.lastLoginAt, u.createdAt)
    }

  def createAddress(r: AddressReq): Res[UUID] =
    val addr = Address.toAscii(r.address.trim).toLowerCase
    val fwd = r.forwardTo.map(f => Address.toAscii(f.trim).toLowerCase).filter(_.nonEmpty).distinct
    if !Address.valid(addr) then IO.pure(Left(Fail.Invalid("メールアドレスが正しくありません")))
    else if fwd.exists(f => !Address.valid(f)) then IO.pure(Left(Fail.Invalid("転送先のアドレスが正しくありません")))
    else if r.userId.isEmpty && fwd.isEmpty then IO.pure(Left(Fail.Invalid("配送先のユーザーか転送先を指定してください")))
    else if fwd.contains(addr) then IO.pure(Left(Fail.Invalid("自分自身へは転送できません")))
    else
      domains.domainByName(Address.domainOf(addr)).flatMap {
        case None    => IO.pure(Left(Fail.Invalid(s"ドメイン ${Address.domainOf(addr)} が登録されていません")))
        case Some(d) => domains.createAddressIO(addr, d.id, r.userId, fwd, false, r.canSend && r.userId.isDefined).orFail
      }
