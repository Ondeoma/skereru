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

/** 外への送信待ち・グレイリスト・送受信の記録・自動返信の記録・振り分けの規則・ベイズの学習。 */
final class QueueStore(xa: Transactor[IO]) extends TxStore(xa) {

  private val ocols =
    fr"id, user_id, sender, recipient, domain, blob_id, status, attempts, next_attempt_at, last_error, created_at"

  def enqueue(userId: Option[UUID], sender: String, recipients: List[String], blobId: String): IO[Int] = tx(
    recipients.distinct.traverse { r =>
      sql"""insert into outbound (id, user_id, sender, recipient, domain, blob_id) values (${UUID.randomUUID()}, $userId, $sender, $r,
            ${skereru.core.Address.domainOf(r)}, $blobId)""".update.run
    }.map(_.sum)
  )

  /**
   * 送る時刻になった宛先を取り出して1件ずつ処理する（同じ本体・同じドメインのものはまとめて渡す）。
   * 取り出しから記録までを1つのトランザクションで行い、ほかのノードには取られない。
   */
  def processDue(limit: Int)(run: List[Outbound] => IO[Map[UUID, OutboxResult]]): IO[Int] =
    processEach(
      (fr"select" ++ ocols ++
        fr"from outbound where status = 'queued' and next_attempt_at <= now() order by next_attempt_at limit $limit for update skip locked")
        .query[Outbound].to[List].map(_.groupBy(o => (o.blobId, o.domain, o.sender)).values.toList)
    )(run) {
      (batch, results) =>
        batch.traverse { o =>
          results.getOrElse(o.id, OutboxResult.Retry("未処理")) match {
            case OutboxResult.Sent =>
              sql"update outbound set status = 'sent', attempts = attempts + 1, last_error = null, done_at = now() where id = ${o.id}".update.run
            case OutboxResult.Skip(reason) =>
              sql"update outbound set status = 'failed', attempts = attempts + 1, last_error = $reason, done_at = now() where id = ${o.id}".update.run
            case OutboxResult.Retry(e) =>
              val secs = Outbox.backoffSeconds(o.attempts)
              sql"""update outbound set attempts = attempts + 1, last_error = $e, next_attempt_at = now() + make_interval(secs => $secs)
                  where id = ${o.id}""".update.run
          }
        }.map(_.sum)
    }

  /** 期限（maxAgeHours）を過ぎても送れていないものを失敗にする。失敗にしたものを返す（差出人への通知用）。 */
  def expire(maxAgeHours: Int): IO[List[Outbound]] =
    tx((fr"update outbound set status = 'failed', done_at = now(), last_error = coalesce(last_error, '') || ' (再送の期限切れ)' where status = 'queued' and created_at < now() - make_interval(hours => $maxAgeHours) returning" ++
      ocols).query[Outbound].to[List])

  /** 直近1時間にユーザーが外へ送った宛先の数（乗っ取られたアカウントからの大量送信を止める）。 */
  def recentRecipients(userId: UUID): IO[Int] =
    tx(sql"select count(*)::int from outbound where user_id = $userId and created_at > now() - interval '1 hour'".query[
      Int
    ].unique)

  def list(status: Option[String], limit: Int): IO[List[QueueItemDto]] =
    tx((fr"select id, sender, recipient, status, attempts, next_attempt_at, last_error, created_at from outbound" ++
      Sql.whereAnd(
        status.map(s => fr"status = $s").toList
      ) ++ fr"order by created_at desc limit $limit").query[QueueItemDto].to[List])

  def retryNow(id: UUID): IO[Int] = tx(
    sql"update outbound set status = 'queued', next_attempt_at = now() where id = $id and status <> 'sent'".update.run
  )
  def cancel(id: UUID): IO[Int] = tx(
    sql"update outbound set status = 'failed', last_error = '管理者が取り消しました', done_at = now() where id = $id and status = 'queued'".update.run
  )

  def purgeDone(days: Int): IO[Int] =
    tx(sql"delete from outbound where status <> 'queued' and done_at < now() - make_interval(days => $days)".update.run)

  // ---------------------------------------------------------------- グレイリスト

  /** 初めての組み合わせなら false（一時拒否する）。delaySecs を過ぎて再送されたら通す。 */
  def greylist(key: String, delaySecs: Int): IO[Boolean] = tx(
    sql"select first_seen, passed from greylist where key = $key".query[(Instant, Boolean)].option.flatMap {
      case None => sql"insert into greylist (key) values ($key) on conflict do nothing".update.run.as(false)
      case Some((_, true)) => FC.pure(true)
      case Some((first, false)) =>
        if first.plusSeconds(delaySecs.toLong).isBefore(Instant.now()) then
          sql"update greylist set passed = true where key = $key".update.run.as(true)
        else FC.pure(false)
    }
  )

  def purgeGreylist(): IO[Int] = tx(
    sql"delete from greylist where (passed and first_seen < now() - interval '60 days') or (not passed and first_seen < now() - interval '2 days')".update.run
  )

  // ---------------------------------------------------------------- 送受信の記録

  def log(direction: String,
          result: String,
          ip: Option[String],
          sender: String,
          recipients: List[String],
          size: Int,
          score: Option[Double],
          detail: String): IO[Unit] = tx(
    sql"""insert into mail_log (direction, result, ip, sender, recipients, size, score, detail)
          values ($direction, $result, $ip, ${sender.take(300)}, ${recipients.mkString(
        ","
      ).take(2000)}, $size, ${score.map(_.toFloat)}, ${detail.take(2000)})""".update.run.void
  )

  def purgeLog(days: Int): IO[Int] =
    tx(sql"delete from mail_log where at < now() - make_interval(days => $days)".update.run)

  // ---------------------------------------------------------------- 自動返信

  /** 同じ相手に days 日以内に送っていなければ記録して true。 */
  def vacationOnce(userId: UUID, sender: String, days: Int): IO[Boolean] = tx(
    sql"""insert into vacation_sent (user_id, sender, sent_at) values ($userId, ${sender.toLowerCase}, now())
          on conflict (user_id, sender) do update set sent_at = now() where vacation_sent.sent_at < now() - make_interval(days => $days)
          returning 1""".query[Int].option.map(_.isDefined)
  )

  // ---------------------------------------------------------------- 振り分けの規則

  private def ruleRow(r: (UUID, String, Boolean, Boolean, Json, Json, Int)): FilterRuleDto =
    FilterRuleDto(
      r._1,
      r._2,
      r._3,
      r._4,
      r._5.as[List[RuleConditionDto]].getOrElse(Nil),
      r._6.as[List[RuleActionDto]].getOrElse(Nil),
      r._7
    )

  def rules(userId: UUID): IO[List[FilterRuleDto]] =
    tx(sql"select id, name, enabled, match_all, conditions, actions, position from filter_rule where user_id = $userId order by position, created_at".query[
      (UUID, String, Boolean, Boolean, Json, Json, Int)
    ].to[List].map(_.map(ruleRow)))

  def createRule(userId: UUID, r: FilterRuleReq): IO[FilterRuleDto] = tx(
    sql"""insert into filter_rule (id, user_id, name, position, enabled, match_all, conditions, actions)
          values (${UUID.randomUUID()}, $userId, ${r.name}, (select coalesce(max(position), -1) + 1 from filter_rule where user_id = $userId),
            ${r.enabled}, ${r.matchAll}, ${r.conditions.asJson}, ${r.actions.asJson})
          returning id, name, enabled, match_all, conditions, actions, position""".query[(
      UUID,
      String,
      Boolean,
      Boolean,
      Json,
      Json,
      Int
    )].unique.map(ruleRow)
  )

  def updateRule(userId: UUID, id: UUID, r: FilterRuleReq): IO[Int] = tx(
    sql"""update filter_rule set name = ${r.name}, enabled = ${r.enabled}, match_all = ${r.matchAll}, conditions = ${r.conditions.asJson}, actions = ${r.actions.asJson}
          where id = $id and user_id = $userId""".update.run
  )

  def deleteRule(userId: UUID, id: UUID): IO[Int] =
    tx(sql"delete from filter_rule where id = $id and user_id = $userId".update.run)

  def reorderRules(userId: UUID, ids: List[UUID]): IO[Unit] = tx(ids.zipWithIndex.traverse_((id, i) =>
    sql"update filter_rule set position = $i where id = $id and user_id = $userId".update.run
  ))

  // ---------------------------------------------------------------- ベイズの学習

  /** 学習する（spam=true なら迷惑メールとして）。undo=true のときは前の学習を取り消してから（判定の付け替え）。 */
  def train(userId: UUID, tokens: Set[String], spam: Boolean, undo: Boolean): IO[Unit] = tx {
    val toks = tokens.toList
    val (ds, dh) = if spam then (1, if undo then -1 else 0) else (if undo then -1 else 0, 1)
    List(userId, Learning.Global).traverse_ { u =>
      sql"""insert into bayes_total (user_id, spam, ham) values ($u, ${math.max(0, ds)}, ${math.max(0, dh)})
            on conflict (user_id) do update set spam = greatest(0, bayes_total.spam + $ds), ham = greatest(0, bayes_total.ham + $dh)""".update.run *>
        Update[(UUID, String, Int, Int, Int, Int)](
          """insert into bayes_token (user_id, token, spam, ham) values (?, ?, ?, ?)
             on conflict (user_id, token) do update set spam = greatest(0, bayes_token.spam + ?), ham = greatest(0, bayes_token.ham + ?)"""
        ).updateMany(toks.map(t => (u, t, math.max(0, ds), math.max(0, dh), ds, dh)))
    }
  }

  /** トークンの数と学習した通数（本人の学習が足りなければ全体の学習を使う）。 */
  def bayesCounts(userId: UUID, tokens: Set[String]): IO[(Map[String, (Int, Int)], Int, Int)] = tx {
    def load(u: UUID) =
      for {
        tot <-
          sql"select spam, ham from bayes_total where user_id = $u".query[(Int, Int)].option.map(_.getOrElse((0, 0)))
        cs <- sql"select token, spam, ham from bayes_token where user_id = $u and token = any(${tokens.toList})".query[(
          String,
          Int,
          Int
        )].to[List]
      } yield (
        cs.map((t, s, h) => t -> (s, h)).toMap,
        tot._1,
        tot._2
      )
    load(userId).flatMap { r => if r._2 >= 30 && r._3 >= 30 then FC.pure(r) else load(Learning.Global) }
  }

  def purgeBayes(): IO[Int] = tx(sql"delete from bayes_token where spam + ham <= 1 and random() < 0.1".update.run)
}

object Learning {

  /** 全員で共有する学習の user_id。 */
  val Global: UUID = new UUID(0L, 0L)
}
