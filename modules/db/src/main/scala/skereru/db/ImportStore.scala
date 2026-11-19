package skereru.db

import skereru.core.ErrorCode

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skereru.shared.*

import java.util.UUID

/** IMAP の取り込みで、引き受けたノードが使う行（接続情報は暗号化したまま）。 */
final case class ImapJob(id: UUID, userId: UUID, secret: String)

/** メールの取り込み（import_job）。 */
final class ImportStore(xa: Transactor[IO]) extends TxStore(xa) {

  private val cols = fr"id, kind, status, source, imported, skipped, failed, current, error, created_at, finished_at"

  /** 進行中（queued・running）の取り込みは1人1つまで（同じメールボックスに同時に書き込ませない）。 */
  def create(userId: UUID,
             kind: String,
             source: String,
             secret: Option[String],
             status: String): IO[Either[AppError, ImportJobDto]] = tx(
    // 画面が閉じられて止まったファイルの取り込み（2分以上進んでいない）は、ここで終わらせる（次の取り込みをふさがない）
    sql"""update import_job set status = 'failed', error = '画面が閉じられたため、途中で止まりました', finished_at = now()
          where user_id = $userId and kind = 'file' and status = 'running' and coalesce(heartbeat, created_at) < now() - interval '2 minutes'""".update.run *>
      sql"select count(*) from import_job where user_id = $userId and status in ('queued', 'running')".query[
        Int
      ].unique.flatMap { n =>
        if n > 0 then fail(AppError(ErrorCode.ImportAlreadyRunning))
        else
          (fr"insert into import_job (id, user_id, kind, status, source, secret) values (${UUID.randomUUID()}, $userId, $kind, $status, $source, $secret) returning" ++
            cols)
            .query[ImportJobDto].unique.map(Right(_))
      }
  )

  def list(userId: UUID, limit: Int = 20): IO[List[ImportJobDto]] = tx((fr"select" ++ cols ++
    fr"from import_job where user_id = $userId order by created_at desc limit $limit").query[ImportJobDto].to[List])

  def find(userId: UUID, id: UUID): IO[Option[ImportJobDto]] =
    tx((fr"select" ++ cols ++ fr"from import_job where user_id = $userId and id = $id").query[ImportJobDto].option)

  def status(id: UUID): IO[Option[String]] = tx(sql"select status from import_job where id = $id".query[String].option)

  /** 待っている IMAP の取り込みを1つ引き受ける。引き受けたノードが止まった（heartbeat が途絶えた）ものも引き継ぐ。 */
  def claimImap(node: String): IO[Option[ImapJob]] = tx(
    sql"""update import_job set status = 'running', node = $node, heartbeat = now()
          where id = (select id from import_job
                      where kind = 'imap' and secret is not null
                        and (status = 'queued' or (status = 'running' and heartbeat < now() - interval '2 minutes'))
                      order by created_at limit 1 for update skip locked)
          returning id, user_id, secret""".query[ImapJob].option
  )

  /** 進み具合を足す（heartbeat も更新する）。 */
  def progress(id: UUID, imported: Int, skipped: Int, failed: Int, current: Option[String] = None): IO[Unit] = tx(
    sql"""update import_job set imported = imported + $imported, skipped = skipped + $skipped, failed = failed + $failed,
          current = coalesce($current, current), heartbeat = now() where id = $id""".update.run.void
  )

  /** 終える（接続情報は消す）。取り消されたものは取り消しのまま。 */
  def finish(id: UUID, status: String, error: Option[String]): IO[Unit] = tx(
    sql"""update import_job set status = case when status = 'cancelled' then status else $status end, error = ${error.map(
        _.take(1000)
      )},
          secret = null, current = '', finished_at = now() where id = $id""".update.run.void
  )

  def cancel(userId: UUID, id: UUID): IO[Int] = tx(
    sql"""update import_job set status = 'cancelled', secret = null, finished_at = now()
          where user_id = $userId and id = $id and status in ('queued', 'running')""".update.run
  )

  /** ファイルの取り込みで、画面が閉じられて途中で止まったものを終える。 */
  def expireStale(): IO[Int] = tx(
    sql"""update import_job set status = 'failed', error = '画面が閉じられたため、途中で止まりました', secret = null, finished_at = now()
          where kind = 'file' and status = 'running' and coalesce(heartbeat, created_at) < now() - interval '1 hour'""".update.run
  )

  /** 同じ本体のメッセージが、そのメールボックス（None ならユーザーのどこか）にすでにあるか。 */
  def exists(userId: UUID, mailboxId: Option[UUID], blobId: String): IO[Boolean] =
    tx(mailboxId.fold(sql"select exists (select 1 from message where user_id = $userId and blob_id = $blobId)")(b =>
      sql"select exists (select 1 from message where user_id = $userId and mailbox_id = $b and blob_id = $blobId)"
    ).query[Boolean].unique)
}
