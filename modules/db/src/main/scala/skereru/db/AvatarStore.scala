package skereru.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.util.UUID

/** ユーザーが保存したアイコン. あれば IdP の画像（app_user.avatar_url）より優先して表示する. */
final class AvatarStore(xa: Transactor[IO]) extends TxStore(xa) {

  /** アイコンを保存する（版を1つ進める. 画像の URL に版を付けるので、変えるとすぐ切り替わる）. */
  def save(id: UUID, contentType: String, data: Array[Byte]): IO[Unit] = tx(
    sql"""insert into user_avatar (user_id, content_type, data) values ($id, $contentType, $data)
          on conflict (user_id) do update set content_type = excluded.content_type, data = excluded.data, updated_at = now()""".update.run *>
      sql"update app_user set avatar_version = coalesce(avatar_version, 0) + 1 where id = $id".update.run.void
  )

  /** 保存したアイコンを消す（IdP の画像に戻る）. */
  def delete(id: UUID): IO[Unit] = tx(sql"delete from user_avatar where user_id = $id".update.run *>
    sql"update app_user set avatar_version = null where id = $id".update.run.void)

  def find(id: UUID): IO[Option[(String, Array[Byte])]] =
    tx(sql"select content_type, data from user_avatar where user_id = $id".query[(String, Array[Byte])].option)
}

object AvatarStore {

  /** 表示するアイコンの URL の列（別名 a）. 保存したアイコンがあれば /api/users/{id}/avatar?v={版}、なければ IdP の画像. */
  def url(a: String): Fragment = Fragment.const(
    s"(case when $a.avatar_version is not null then '/api/users/' || $a.id || '/avatar?v=' || $a.avatar_version else $a.avatar_url end)"
  )
}
