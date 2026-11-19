package skereru.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import io.circe.syntax.*
import skereru.core.DmarcAggregate
import skereru.shared.*

import java.util.UUID

/** 受け取った DMARC の集計レポート。 */
final class DmarcStore(xa: Transactor[IO]) extends TxStore(xa) {

  private val cols =
    fr"id, domain, org_name, report_id, policy, begin_at, end_at, total, passing, authenticated, received_at"

  /** 保存する（同じ報告元・同じ ID の報告は1つだけ）。 */
  def save(a: DmarcAggregate, authenticated: Boolean): IO[Unit] = {
    val rows = a.rows.map(r =>
      DmarcRowDto(
        r.sourceIp,
        r.count,
        r.disposition,
        r.dkim,
        r.spf,
        r.headerFrom,
        r.envelopeFrom,
        r.dkimDomains,
        r.spfDomains
      )
    ).asJson
    tx(sql"""insert into dmarc_report (id, domain, org_name, email, report_id, policy, begin_at, end_at, total, passing, rows, authenticated)
            values (${UUID.randomUUID()}, ${a.domain.toLowerCase}, ${a.orgName}, ${a.email}, ${a.reportId}, ${a.policy}, ${a.begin}, ${a.end},
              ${a.total}, ${a.passing}, $rows, $authenticated)
            on conflict (domain, org_name, report_id) do nothing""".update.run.void)
  }

  def list(domain: Option[String], limit: Int = 200): IO[List[DmarcReportDto]] = tx(
    (fr"select" ++ cols ++ fr"from dmarc_report" ++
      domain.fold(Fragment.empty)(d => fr"where domain = ${d.toLowerCase}") ++
      fr"order by end_at desc limit $limit").query[DmarcReportDto].to[List]
  )

  def rows(id: UUID): IO[Option[List[DmarcRowDto]]] =
    tx(sql"select rows from dmarc_report where id = $id".query[Json].option.map(_.flatMap(
      _.as[List[DmarcRowDto]].toOption
    )))

  def purge(days: Int): IO[Int] =
    tx(sql"delete from dmarc_report where received_at < now() - make_interval(days => $days)".update.run)
}
