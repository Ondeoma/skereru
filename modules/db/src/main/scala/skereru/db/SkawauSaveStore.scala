package skereru.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import sbirds.db.{Outbox, OutboxResult, TxStore}

import java.util.UUID

/** 保存待ちの1件（その人のメッセージの添付を skawau に保存する）. */
final case class SkawauSaveRow(id: UUID, userId: UUID, messageId: UUID, attempts: Int)

/** 振り分けの規則の「添付を skawau に保存」の送信待ちの表. */
final class SkawauSaveStore(xa: Transactor[IO]) extends TxStore(xa) {

  /** 送り直す回数の上限. */
  val MaxAttempts = 8

  def enqueue(userId: UUID, messageId: UUID): IO[Unit] = tx(
    sql"insert into skawau_save (id, user_id, message_id) values (${UUID.randomUUID()}, $userId, $messageId)".update.run.void
  )

  /** 時刻が来たものを、ほかのノードと取り合わずに（SKIP LOCKED）保存する. Skip（送り直しても無駄）なら、その場でやめる. */
  def process(batch: Int)(save: SkawauSaveRow => IO[OutboxResult]): IO[Int] =
    processEach(
      sql"""select id, user_id, message_id, attempts from skawau_save
            where done_at is null and not failed and not_before <= now()
            order by not_before limit $batch for update skip locked""".query[SkawauSaveRow].to[List]
    )(r => save(r).handleError(e => OutboxResult.Retry(e.getClass.getSimpleName))) {
      case (r, OutboxResult.Sent) =>
        sql"update skawau_save set done_at = now(), attempts = attempts + 1, last_error = null where id = ${r.id}".update.run
      case (r, OutboxResult.Skip(m)) =>
        sql"update skawau_save set failed = true, last_error = $m where id = ${r.id}".update.run
      case (r, OutboxResult.Retry(m)) =>
        sql"""update skawau_save set attempts = attempts + 1, last_error = $m, failed = ${r.attempts + 1 >=
            MaxAttempts},
                not_before = now() + make_interval(secs => ${Outbox.backoffSeconds(
            r.attempts
          )}) where id = ${r.id}""".update.run
    }

  /** その人の、最後に保存をやめたものの理由（7日以内. 後で成功していれば出さない）. 設定の画面に出す. */
  def lastFailure(userId: UUID): IO[Option[String]] = tx(
    sql"""select last_error from skawau_save f
          where f.user_id = $userId and f.failed and f.created_at > now() - interval '7 days'
            and not exists (select 1 from skawau_save d where d.user_id = $userId and d.done_at is not null and d.created_at > f.created_at)
          order by f.created_at desc limit 1""".query[Option[String]].option.map(_.flatten)
  )

  def purge(days: Int): IO[Int] = tx(
    sql"delete from skawau_save where created_at < now() - make_interval(days => $days) and (done_at is not null or failed)".update.run
  )
}
