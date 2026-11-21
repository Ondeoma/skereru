package skereru.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import sbirds.db.{Outbox, OutboxResult, TxStore}
import skereru.shared.*

import java.time.Instant
import java.util.UUID

/** 通知先の行（secretEnc は webhook の署名の鍵を暗号化したもの）. */
final case class NotifyTargetRow(id: UUID,
                                 userId: UUID,
                                 name: String,
                                 kind: String,
                                 channelId: Option[String],
                                 channelName: Option[String],
                                 url: Option[String],
                                 secretEnc: Option[String],
                                 allInbox: Boolean,
                                 enabled: Boolean,
                                 lastError: Option[String],
                                 createdAt: Instant)

/** 送信待ちの通知（送るときの通知先を付けて取り出す）. */
final case class NotifyDeliveryRow(id: UUID, attempts: Int, payload: Json, target: NotifyTargetRow)

/** 受信の通知先（stui・受信 Webhook・署名付きの Webhook）と、通知の送信待ちの表. */
final class NotifyStore(xa: Transactor[IO]) extends TxStore(xa) {

  /** 1人あたりの通知先の数の上限. */
  val MaxTargets = 20

  /** 送り直す回数の上限（それを超えたら送るのをやめる）. */
  val MaxAttempts = 8

  private val cols =
    fr"select t.id, t.user_id, t.name, t.kind, t.channel_id, t.channel_name, t.url, t.secret_enc, t.all_inbox, t.enabled, t.last_error, t.created_at from notify_target t"

  def targets(userId: UUID): IO[List[NotifyTargetRow]] =
    tx((cols ++ fr"where t.user_id = $userId order by t.created_at").query[NotifyTargetRow].to[List])

  def target(userId: UUID, id: UUID): IO[Option[NotifyTargetRow]] =
    tx((cols ++ fr"where t.id = $id and t.user_id = $userId").query[NotifyTargetRow].option)

  def count(userId: UUID): IO[Int] =
    tx(sql"select count(*) from notify_target where user_id = $userId".query[Int].unique)

  def insert(t: NotifyTargetRow): IO[Unit] = tx(
    sql"""insert into notify_target (id, user_id, name, kind, channel_id, channel_name, url, secret_enc, all_inbox, enabled)
          values (${t.id}, ${t.userId}, ${t.name}, ${t.kind}, ${t.channelId}, ${t.channelName}, ${t.url}, ${t.secretEnc},
                  ${t.allInbox}, ${t.enabled})""".update.run.void
  )

  /** 名前・送り先・範囲・有効を変える（種類と署名の鍵は変えない）. 送り先を変えたら、前の失敗の理由は消す. */
  def update(userId: UUID, id: UUID, r: NotifyTargetReq): IO[Boolean] = tx(
    sql"""update notify_target set name = ${r.name}, channel_id = ${r.channelId}, channel_name = ${r.channelName},
            url = ${r.url}, all_inbox = ${r.allInbox}, enabled = ${r.enabled}, last_error = null
          where id = $id and user_id = $userId""".update.run.map(_ > 0)
  )

  def delete(userId: UUID, id: UUID): IO[Boolean] =
    tx(sql"delete from notify_target where id = $id and user_id = $userId".update.run.map(_ > 0))

  /**
   * 新着を知らせる通知先: 有効なもののうち、受信トレイに入ったなら allInbox のものと、規則で選んだもの（ids）.
   * 規則の ID はほかの人の通知先を指していても拾わない（user_id で絞る）.
   */
  def targetsFor(userId: UUID, inbox: Boolean, ids: List[UUID]): IO[List[NotifyTargetRow]] = {
    val byRule = ids.toNel.fold(fr"false")(n => Fragments.in(fr"t.id", n))
    tx((cols ++ fr"where t.user_id = $userId and t.enabled and ((${inbox} and t.all_inbox) or" ++ byRule ++ fr")")
      .query[NotifyTargetRow].to[List])
  }

  def enqueue(targetId: UUID, payload: Json): IO[Unit] = tx(
    sql"insert into notify_delivery (id, target_id, payload) values (${UUID.randomUUID()}, $targetId, $payload)".update.run.void
  )

  /**
   * 送る時刻が来たものを、ほかのノードと取り合わずに（SKIP LOCKED）送る. 送れなかった理由は通知先にも残す（画面に出す）.
   * Skip（送り直しても無駄）なら、その場で送るのをやめる.
   */
  def processDeliveries(batch: Int)(send: NotifyDeliveryRow => IO[(OutboxResult, Option[Int])]): IO[Int] =
    processEach(
      sql"""select d.id, d.attempts, d.payload, t.id, t.user_id, t.name, t.kind, t.channel_id, t.channel_name, t.url, t.secret_enc,
                   t.all_inbox, t.enabled, t.last_error, t.created_at
            from notify_delivery d join notify_target t on t.id = d.target_id
            where d.done_at is null and not d.failed and d.not_before <= now()
            order by d.not_before limit $batch for update of d skip locked"""
        .query[NotifyDeliveryRow].to[List]
    )(d => send(d).handleError(e => (OutboxResult.Retry(e.getClass.getSimpleName), None))) {
      case (d, (OutboxResult.Sent, code)) =>
        sql"update notify_delivery set done_at = now(), attempts = attempts + 1, status_code = $code where id = ${d.id}".update.run <*
          sql"update notify_target set last_error = null where id = ${d.target.id} and last_error is not null".update.run
      case (d, (OutboxResult.Skip(r), code)) =>
        sql"update notify_delivery set failed = true, last_error = $r, status_code = $code where id = ${d.id}".update.run <*
          sql"update notify_target set last_error = $r where id = ${d.target.id}".update.run
      case (d, (OutboxResult.Retry(err), code)) =>
        val giveUp = d.attempts + 1 >= MaxAttempts
        sql"""update notify_delivery set attempts = attempts + 1, last_error = $err, status_code = $code, failed = $giveUp,
                not_before = now() + make_interval(secs => ${Outbox.backoffSeconds(
            d.attempts
          )}) where id = ${d.id}""".update.run <*
          sql"update notify_target set last_error = $err where id = ${d.target.id}".update.run.whenA(giveUp)
    }

  /** 送り終えた・やめた送信待ちを消す（days 日より前のもの）. */
  def purge(days: Int): IO[Int] = tx(
    sql"delete from notify_delivery where created_at < now() - make_interval(days => $days) and (done_at is not null or failed)".update.run
  )
}
