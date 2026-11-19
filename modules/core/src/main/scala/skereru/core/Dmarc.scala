package skereru.core

/** DMARC（RFC 7489）。 */
final class Dmarc(dns: Dns) {

  def record(domain: String): Either[String, Option[Map[String, String]]] =
    dns.txt(s"_dmarc.$domain").map(_.find(_.toLowerCase.replace(" ", "").startsWith("v=dmarc1")).map(r =>
      new Dkim(dns).tags(r)
    ))

  def evaluate(fromDomain: String, spf: SpfResult, mailFromDomain: String, dkim: List[DkimResult]): DmarcResult = {
    val org = OrgDomain.of(fromDomain)
    val rec = record(fromDomain).flatMap {
      case Some(r) => Right(Some(r -> false))
      case None if org != fromDomain => record(org).map(_.map(_ -> true))
      case None => Right(None)
    }
    rec match {
      case Left(_) => DmarcResult("temperror", "none", fromDomain)
      case Right(None) => DmarcResult("none", "none", fromDomain)
      case Right(Some((tags, fromOrg))) =>
        val policy =
          (if fromOrg then tags.get("sp").orElse(tags.get("p")) else tags.get("p")).map(_.toLowerCase).filter(Set(
            "none",
            "quarantine",
            "reject"
          )).getOrElse("none")
        def aligned(d: String, strict: Boolean) =
          if strict then d.equalsIgnoreCase(fromDomain) else OrgDomain.of(d) == org
        val spfOk = spf == SpfResult.Pass && aligned(mailFromDomain, tags.get("aspf").contains("s"))
        val dkimOk = dkim.exists(r => r.pass && aligned(r.domain, tags.get("adkim").contains("s")))
        DmarcResult(if spfOk || dkimOk then "pass" else "fail", policy, fromDomain)
    }
  }
}
