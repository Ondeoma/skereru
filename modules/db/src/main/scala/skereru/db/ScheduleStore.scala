package skereru.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import io.circe.syntax.*
import skereru.shared.*

import java.time.Instant
import java.util.UUID

/** 時刻が来て送る予約（引き受けたノードが送る）。 */
final case class DueSend(
  id: UUID,
  userId: UUID,
  req: ComposeReq,
  ip: Option[String])

/** 予約送信・送信の取り消し（scheduled_send）。 */
final class ScheduleStore(
  xa: Transactor[IO]) extends TxStore(xa) {

  private def dto(
    id: UUID,
    sendAt: Instant,
    req: Json,
    status: String,
    error: Option[String]
  ): ScheduledDto = {
    val r = req.as[ComposeReq].toOption
    ScheduledDto(
      id,
      sendAt,
      r.map(_.subject).getOrElse(""),
      r.map(x => x.to ++ x.cc ++ x.bcc).getOrElse(Nil),
      status,
      error
    )
  }

  def create(
    userId: UUID,
    sendAt: Instant,
    req: ComposeReq,
    ip: Option[String]
  ): IO[ScheduledDto] = {
    val id = UUID.randomUUID()
    tx(
      sql"""insert into scheduled_send (id, user_id, send_at, req, upload_ids, ip) values ($id, $userId, $sendAt, ${req.asJson}, ${req.uploadIds}, $ip)""".update.run
    )
      .as(dto(id, sendAt, req.asJson, "queued", None))
  }

  /** 送る前のものと、最近失敗したもの。 */
  def list(
    userId: UUID
  ): IO[List[ScheduledDto]] = tx(
    sql"""select id, send_at, req, status, error from scheduled_send
          where user_id = $userId and (status = 'queued' or (status = 'failed' and created_at > now() - interval '7 days'))
          order by send_at""".query[(UUID, Instant, Json, String, Option[String])].to[List].map(_.map(dto.tupled))
  )

  /** 取り消す（送る前のものだけ）。作成画面に戻すための内容を返す。 */
  def cancel(
    userId: UUID,
    id: UUID
  ): IO[Option[ComposeReq]] = tx(
    sql"""update scheduled_send set status = 'cancelled' where id = $id and user_id = $userId and status in ('queued', 'failed') returning req"""
      .query[Json].option.map(_.flatMap(_.as[ComposeReq].toOption))
  )

  /** 時刻が来たものを引き受ける（ほかのノードと重ならないよう SKIP LOCKED。引き受けたら sending にする）。 */
  def claimDue(
    limit: Int = 20
  ): IO[List[DueSend]] = tx(
    sql"""update scheduled_send set status = 'sending'
          where id in (select id from scheduled_send where status = 'queued' and send_at <= now() order by send_at limit $limit for update skip locked)
          returning id, user_id, req, ip""".query[(UUID, UUID, Json, Option[String])].to[List]
      .map(_.flatMap(
        (
          id,
          u,
          j,
          ip
        ) => j.as[ComposeReq].toOption.map(DueSend(id, u, _, ip))
      ))
  )

  def finish(
    id: UUID,
    error: Option[String]
  ): IO[Unit] = tx(
    sql"""update scheduled_send set status = ${if error.isEmpty then "sent" else "failed"}, error = ${error.map(
        _.take(500)
      )}, sent_at = now()
          where id = $id""".update.run.void
  )

  /** 送っている途中でノードが止まったもの（二重に送らないよう、送り直さずに失敗にする）と、古い記録を片付ける。 */
  def cleanup(): IO[Unit] = tx(
    sql"""update scheduled_send set status = 'failed', error = '送信の途中でサーバーが止まりました。送信済みを確かめてください'
          where status = 'sending' and send_at < now() - interval '10 minutes'""".update.run *>
      sql"delete from scheduled_send where status in ('sent', 'cancelled') and created_at < now() - interval '30 days'".update.run.void
  )
}
