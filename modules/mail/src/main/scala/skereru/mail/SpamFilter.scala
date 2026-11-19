package skereru.mail

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import skereru.core.*
import skereru.db.*
import skereru.shared.*

import java.net.InetAddress
import java.time.Instant
import java.util.UUID

/** 受信したメッセージの迷惑メール判定（送信ドメイン認証・DNSBL・ベイズ・ウイルス・規則）。 */
final class SpamFilter(
  dns: JndiDns,
  queue: QueueStore,
  cfg: MailConfig,
  log: String => Unit) {
  private val spf = new Spf(dns)
  private val dkim = new Dkim(dns)
  private val dmarc = new Dmarc(dns)
  private val clam = cfg.clamav.map(
    (
      h,
      p
    ) => new ClamAv(h, p)
  )

  final case class Result(
    verdict: SpamVerdict,
    auth: AuthResultDto,
    header: String,
    json: Json)

  def check(
    raw: Array[Byte],
    root: MimePart,
    ip: InetAddress,
    helo: String,
    mailFrom: String,
    bayesUser: Option[UUID],
    isLocalDomain: String => Boolean
  ): IO[Result] = {
    val fromDomain = root.header("From").flatMap(Address.parseOne).map(_.domain).getOrElse("")
    val mfDomain = if mailFrom.isEmpty then helo else Address.domainOf(mailFrom)
    // 互いに関係しない問い合わせは並列にする（DMARC だけは SPF・DKIM の結果を使う）
    val authIO =
      (IO.blocking(spf.check(ip, mailFrom, helo)), IO.blocking(dkim.verify(raw, Instant.now()))).parTupled.flatMap {
        (
          spfRes,
          dkimRes
        ) =>
          IO.blocking(if fromDomain.isEmpty then DmarcResult("none", "none", "")
          else dmarc.evaluate(fromDomain, spfRes, mfDomain, dkimRes))
            .map(d => (spfRes, dkimRes, d))
      }
    val rdnsIO = IO.blocking(dns.verifiedReverse(ip))
    val blIO = IO.blocking(if ip.isLoopbackAddress || ip.isSiteLocalAddress then Nil else dns.dnsbl(ip, cfg.dnsblZones))
    val virusIO = IO.blocking(clam.flatMap(c =>
      scala.util.Try(c.scan(raw)).fold(e => { log(s"[spam] clamd: $e"); None }, identity)
    ))
    (authIO, rdnsIO, blIO, virusIO).parTupled.flatMap {
      case ((spfRes, dkimRes, dmarcRes), rdns, bl, virus) => {
        val toks = Bayes.tokens(raw, root)
        bayesUser.traverse(u => queue.bayesCounts(u, toks)).map(_.flatMap(
          (
            c,
            s,
            h
          ) => skereru.core.Bayes.classify(c, s, h)
        )).map { bayes =>
          {
            val in = SpamInput(
              raw,
              root,
              Some(ip),
              helo,
              rdns,
              mailFrom,
              spfRes,
              dkimRes,
              dmarcRes,
              bl,
              bayes,
              // From のどれか1つでもこのサーバーのドメインなら、名乗っているとみなす
              claimsLocalDomain =
                SpamRules.fromAddresses(root).exists(a => a.domain.nonEmpty && isLocalDomain(a.domain)),
              virus = virus,
              now = Instant.now()
            )
            val v = SpamRules.evaluate(in, cfg.thresholds)
            val dkimStr = dkimRes.find(_.pass).map(r => s"pass (d=${r.domain})").orElse(dkimRes.headOption.map(r =>
              s"${r.result} (d=${r.domain})"
            )).getOrElse("none")
            val auth = AuthResultDto(
              s"${spfRes.label} (${if mailFrom.isEmpty then "helo=" + helo else "smtp.mailfrom=" + mailFrom})",
              dkimStr,
              s"${dmarcRes.result} (p=${dmarcRes.policy} header.from=$fromDomain)"
            )
            val header =
              s"Authentication-Results: ${cfg.hostname};\r\n\tspf=${spfRes.label} smtp.mailfrom=${
                  if mailFrom.isEmpty then "<>" else mailFrom
                };\r\n\t" +
                (if dkimRes.isEmpty then "dkim=none"
                 else
                   dkimRes.map(r => s"dkim=${r.result} header.d=${r.domain} header.s=${r.selector}").mkString(
                     ";\r\n\t"
                   )) +
                s";\r\n\tdmarc=${dmarcRes.result} (p=${dmarcRes.policy}) header.from=$fromDomain\r\n"
            Result(v, auth, header, auth.asJson)
          }
        }
      }
    }
  }
}
