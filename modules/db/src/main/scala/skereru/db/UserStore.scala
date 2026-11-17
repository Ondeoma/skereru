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

/** ユーザー・グループ・セッション・ログイン途中の状態・アプリパスワード・APIキー・総当たり対策・監査ログ・宛先の候補。 */
final class UserStore(xa: Transactor[IO]) extends TxStore(xa):

  private val cols = fr"u.id, u.oidc_sub, u.name, u.email, u.avatar_url, u.is_admin, u.disabled, u.timezone, u.quota_bytes, u.used_bytes, u.created_at, u.last_login_at"

  // ---------------------------------------------------------------- ユーザー

  /** IdP でログインした人のユーザー。sub が一致する人、なければ管理者が先に作った（email が一致する未結び付けの）ユーザー、なければ新規。 */
  /** email は IdP が確かめたもの（emailVerified）だけを渡すこと。管理者が作ったユーザーへの結び付けに使う。 */
  def login(sub: String, name: String, email: Option[String], avatar: Option[String], create: Boolean): IO[Option[User]] = tx(
    for
      bySub <- (fr"select" ++ cols ++ fr"from app_user u where u.oidc_sub = $sub").query[User].option
      res <- bySub match
        case Some(u) =>
          sql"""update app_user set name = $name, email = $email, avatar_url = coalesce($avatar, avatar_url), last_login_at = now() where id = ${u.id}""".update.run *>
            findC(u.id)
        case None =>
          email.traverse(e => (fr"select" ++ cols ++ fr"from app_user u where u.oidc_sub is null and lower(u.email) = lower($e) for update").query[User].option)
            .map(_.flatten).flatMap {
              case Some(u) =>
                sql"""update app_user set oidc_sub = $sub, name = coalesce(nullif(name, ''), $name), avatar_url = $avatar, last_login_at = now()
                      where id = ${u.id}""".update.run *> findC(u.id)
              case None if create =>
                val id = UUID.randomUUID()
                sql"""insert into app_user (id, oidc_sub, name, email, avatar_url, last_login_at) values ($id, $sub, $name, $email, $avatar, now())""".update.run *>
                  findC(id)
              case None => FC.pure(None)
            }
    yield res
  )

  def find(id: UUID): IO[Option[User]] = tx(findC(id))
  def findC(id: UUID): ConnectionIO[Option[User]] = (fr"select" ++ cols ++ fr"from app_user u where u.id = $id").query[User].option

  /** 管理者がメールのユーザーを作る（IdP の email で最初にログインした人に結び付く）。 */
  def create(name: String, email: Option[String], quota: Option[Long]): ConnectionIO[UUID] =
    val id = UUID.randomUUID()
    sql"insert into app_user (id, name, email, quota_bytes) values ($id, $name, $email, $quota)".update.run.as(id)

  /** 開発用のログイン（名前で探し、なければ作る）。 */
  def devUser(name: String, email: Option[String], admin: Boolean): IO[User] = tx(
    for
      id <- sql"""insert into app_user (id, oidc_sub, name, email, is_admin, last_login_at) values (${UUID.randomUUID()}, ${"dev:" + name}, $name, $email, $admin, now())
                  on conflict (oidc_sub) do update set last_login_at = now(), is_admin = app_user.is_admin or excluded.is_admin
                  returning id""".query[UUID].unique
      u <- findC(id)
    yield u.get
  )

  def list(q: Option[String], limit: Int): IO[List[User]] = tx {
    val f = q.map(_.trim).filter(_.nonEmpty).map(s => s"%${Like.escape(s.toLowerCase)}%")
      .map(p => fr"where lower(u.name) like $p or lower(coalesce(u.email, '')) like $p or exists (select 1 from address a where a.user_id = u.id and a.address like $p)")
      .getOrElse(Fragment.empty)
    (fr"select" ++ cols ++ fr"from app_user u" ++ f ++ fr"order by u.name limit $limit").query[User].to[List]
  }

  def update(id: UUID, name: Option[String], isAdmin: Option[Boolean], disabled: Option[Boolean], quota: Option[Option[Long]]): IO[Int] = tx {
    val sets = List(
      name.map(n => fr"name = $n"), isAdmin.map(a => fr"is_admin = $a"), disabled.map(d => fr"disabled = $d"), quota.map(q => fr"quota_bytes = $q")
    ).flatten
    if sets.isEmpty then FC.pure(0)
    else
      (fr"update app_user" ++ Sql.set(sets) ++ fr"where id = $id").update.run.flatTap { _ =>
        // 無効にしたらセッションを消す
        if disabled.contains(true) then sql"delete from session where user_id = $id".update.run.void else FC.unit
      }
  }

  def updateMe(id: UUID, name: Option[String], timezone: Option[String]): IO[Unit] = tx {
    val sets = List(name.map(_.trim).filter(_.nonEmpty).map(n => fr"name = ${n.take(100)}"), timezone.map(t => fr"timezone = $t")).flatten
    if sets.isEmpty then FC.unit else (fr"update app_user" ++ Sql.set(sets) ++ fr"where id = $id").update.run.void
  }

  def delete(id: UUID): IO[Int] = tx(sql"delete from app_user where id = $id".update.run)

  def groupsIO(id: UUID): IO[List[String]] = tx(groups(id))
  def groups(id: UUID): ConnectionIO[List[String]] = sql"select name from user_group where user_id = $id order by name".query[String].to[List]

  def withGroups(id: UUID): IO[Option[(User, List[String])]] = tx(findC(id).flatMap(_.traverse(u => groups(id).map(u -> _))))

  def syncGroups(id: UUID, names: List[String]): IO[Unit] = tx(
    sql"delete from user_group where user_id = $id".update.run *>
      names.map(_.trim).filter(_.nonEmpty).distinct.traverse_(n => sql"insert into user_group (user_id, name) values ($id, $n)".update.run)
  )

  def settings(id: UUID): IO[MailSettingsDto] = tx(settingsC(id))
  def settingsC(id: UUID): ConnectionIO[MailSettingsDto] =
    sql"select settings from app_user where id = $id".query[Json].option.map(_.flatMap { j =>
      // 項目が足りない古い設定は既定値で補う
      MailSettingsDto.default.asJson.deepMerge(j).as[MailSettingsDto].toOption
    }.getOrElse(MailSettingsDto.default))

  def saveSettings(id: UUID, s: MailSettingsDto): IO[Unit] = tx(sql"update app_user set settings = ${s.asJson} where id = $id".update.run.void)

  // ---------------------------------------------------------------- セッション

  def createSession(hash: Array[Byte], userId: UUID, expires: Instant, ip: Option[String], ua: Option[String]): IO[Unit] = tx(
    sql"insert into session (token_hash, user_id, expires_at, ip, user_agent) values ($hash, $userId, $expires, $ip, ${ua.map(_.take(300))})".update.run.void
  )

  /** 有効なセッションのユーザー（無効にされたユーザーは除く）。 */
  def findSession(hash: Array[Byte], now: Instant): IO[Option[(User, List[String])]] = tx(
    (fr"select" ++ cols ++ fr"from session s join app_user u on u.id = s.user_id where s.token_hash = $hash and s.expires_at > $now and not u.disabled")
      .query[User].option.flatMap(_.traverse(u => groups(u.id).map(u -> _)))
  )

  def deleteSession(hash: Array[Byte]): IO[Unit] = tx(sql"delete from session where token_hash = $hash".update.run.void)
  def deleteSessions(userId: UUID): IO[Unit] = tx(sql"delete from session where user_id = $userId".update.run.void)

  def saveLoginState(state: String, verifier: String, nonce: String, returnTo: String): IO[Unit] = tx(
    sql"insert into login_state (state, verifier, nonce, return_to) values ($state, $verifier, $nonce, $returnTo)".update.run.void
  )

  /** ログイン途中の状態を取り出す（1回だけ使える。10分で期限切れ）。 */
  def takeLoginState(state: String): IO[Option[(String, String, String)]] = tx(
    sql"""delete from login_state where state = $state and created_at > now() - interval '10 minutes' returning verifier, nonce, return_to"""
      .query[(String, String, String)].option
  )

  def purgeExpired(now: Instant): IO[Unit] = tx(
    sql"delete from session where expires_at < $now".update.run *>
      sql"delete from login_state where created_at < now() - interval '1 hour'".update.run *>
      sql"delete from auth_failure where window_start < now() - interval '1 day' and (locked_until is null or locked_until < now())".update.run.void
  )

  // ---------------------------------------------------------------- アプリパスワード

  def appPasswords(userId: UUID): IO[List[AppPasswordDto]] = tx(
    sql"""select id, name, prefix, created_at, last_used_at, last_used_ip from app_password where user_id = $userId order by created_at desc"""
      .query[AppPasswordDto].to[List]
  )

  def createAppPassword(userId: UUID, name: String, hash: Array[Byte], prefix: String): IO[AppPasswordDto] = tx(
    sql"""insert into app_password (id, user_id, name, token_hash, prefix) values (${UUID.randomUUID()}, $userId, $name, $hash, $prefix)
          returning id, name, prefix, created_at, last_used_at, last_used_ip""".query[AppPasswordDto].unique
  )

  def deleteAppPassword(userId: UUID, id: UUID): IO[Int] = tx(sql"delete from app_password where id = $id and user_id = $userId".update.run)

  /** アプリパスワードで認証する（ユーザー名はそのユーザーのアドレスのどれか）。 */
  def checkAppPassword(login: String, hash: Array[Byte], ip: String): IO[Option[(User, UUID)]] = tx(
    (fr"select" ++ cols ++ fr""", p.id from app_password p join app_user u on u.id = p.user_id
          where p.token_hash = $hash and not u.disabled
            and (exists (select 1 from address a where a.user_id = u.id and a.address = ${login.toLowerCase.trim}) or lower(coalesce(u.email, '')) = ${login.toLowerCase.trim})""")
      .query[(User, UUID)].option.flatTap(_.traverse_((_, pid) => sql"update app_password set last_used_at = now(), last_used_ip = $ip where id = $pid and (last_used_at is null or last_used_at < now() - interval '1 minute' or last_used_ip is distinct from $ip)".update.run))
  )

  // ---------------------------------------------------------------- APIキー

  def apiKeys(userId: UUID): IO[List[ApiKeyDto]] = tx(
    sql"select id, name, prefix, scopes, created_at, expires_at, last_used_at from api_key where user_id = $userId order by created_at desc"
      .query[ApiKeyDto].to[List]
  )

  def createApiKey(userId: UUID, name: String, hash: Array[Byte], prefix: String, scopes: List[String], expires: Option[Instant]): IO[ApiKeyDto] = tx(
    sql"""insert into api_key (id, user_id, name, key_hash, prefix, scopes, expires_at) values (${UUID.randomUUID()}, $userId, $name, $hash, $prefix, $scopes, $expires)
          returning id, name, prefix, scopes, created_at, expires_at, last_used_at""".query[ApiKeyDto].unique
  )

  def deleteApiKey(userId: UUID, id: UUID): IO[Int] = tx(sql"delete from api_key where id = $id and user_id = $userId".update.run)

  def findApiKey(hash: Array[Byte], now: Instant): IO[Option[(User, List[String], UUID, List[String])]] = tx(
    (fr"select" ++ cols ++ fr""", k.id, k.scopes from api_key k join app_user u on u.id = k.user_id
          where k.key_hash = $hash and not u.disabled and (k.expires_at is null or k.expires_at > $now)""")
      .query[(User, UUID, List[String])].option.flatMap(_.traverse { (u, kid, scopes) =>
        // 最終利用日時は1分に1回だけ書く
        sql"update api_key set last_used_at = now() where id = $kid and (last_used_at is null or last_used_at < now() - interval '1 minute')".update.run *>
          groups(u.id).map(gs => (u, gs, kid, scopes))
      })
  )

  // ---------------------------------------------------------------- 総当たり対策

  /** key（ip:… や user:…）がロックされているか。 */
  def lockedUntil(key: String, now: Instant): IO[Option[Instant]] = tx(
    sql"select locked_until from auth_failure where key = $key and locked_until > $now".query[Instant].option
  )

  /** 失敗を記録する。window 内に max 回を超えたら lock の間ロックする。戻り値はロックしたか。 */
  def recordFailure(key: String, max: Int, windowSecs: Int, lockSecs: Int): IO[Boolean] = tx(
    sql"""insert into auth_failure (key, failures, window_start) values ($key, 1, now())
          on conflict (key) do update set
            failures = case when auth_failure.window_start < now() - make_interval(secs => $windowSecs) then 1 else auth_failure.failures + 1 end,
            window_start = case when auth_failure.window_start < now() - make_interval(secs => $windowSecs) then now() else auth_failure.window_start end
          returning failures""".query[Int].unique.flatMap { n =>
      if n >= max then sql"update auth_failure set locked_until = now() + make_interval(secs => $lockSecs) where key = $key".update.run.as(true)
      else FC.pure(false)
    }
  )

  def clearFailures(key: String): IO[Unit] = tx(sql"delete from auth_failure where key = $key and locked_until is null".update.run.void)

  // ---------------------------------------------------------------- 監査ログ

  def audit(userId: Option[UUID], via: String, action: String, target: Option[String], ip: Option[String], detail: Json = Json.obj()): IO[Unit] = tx(
    sql"insert into audit_log (user_id, via, action, target, ip, detail) values ($userId, $via, $action, $target, $ip, $detail)".update.run.void
  )

  def auditLog(limit: Int, before: Option[Long], action: Option[String]): IO[List[AuditDto]] = tx {
    val conds = List(before.map(b => fr"l.id < $b"), action.map(a => fr"l.action like ${Like.escape(a) + "%"}")).flatten
    (fr"select l.id, l.at, u.name, l.via, l.action, l.target, l.ip, l.detail from audit_log l left join app_user u on u.id = l.user_id" ++
      Sql.whereAnd(conds) ++ fr"order by l.id desc limit $limit").query[AuditDto].to[List]
  }

  def purgeAudit(days: Int): IO[Int] = tx(sql"delete from audit_log where at < now() - make_interval(days => $days)".update.run)

  // ---------------------------------------------------------------- 宛先の候補

  def touchContacts(userId: UUID, addrs: List[(String, Option[String])]): IO[Unit] = tx(
    addrs.distinctBy(_._1.toLowerCase).take(50).traverse_ { (a, n) =>
      sql"""insert into contact (user_id, address, name) values ($userId, ${a.toLowerCase}, $n)
            on conflict (user_id, address) do update set uses = contact.uses + 1, last_used = now(), name = coalesce(excluded.name, contact.name)""".update.run
    }
  )

  def contacts(userId: UUID, q: String, limit: Int): IO[List[ContactDto]] = tx {
    val p = s"%${Like.escape(q.trim.toLowerCase)}%"
    // 自分で保存した連絡先を先に出す
    sql"""select address, name, uses from contact where user_id = $userId and (address like $p or lower(coalesce(name, '')) like $p)
          order by saved desc, uses desc, last_used desc limit $limit""".query[ContactDto].to[List]
  }

  // ---------------------------------------------------------------- 兄弟のサービスとの連携（secret は暗号化したもの）

  def integration(userId: UUID, kind: String): IO[Option[String]] =
    tx(sql"select secret from user_integration where user_id = $userId and kind = $kind".query[String].option)
  def setIntegration(userId: UUID, kind: String, secret: String): IO[Unit] = tx(
    sql"""insert into user_integration (user_id, kind, secret) values ($userId, $kind, $secret)
          on conflict (user_id, kind) do update set secret = excluded.secret, created_at = now()""".update.run.void)
  def deleteIntegration(userId: UUID, kind: String): IO[Int] =
    tx(sql"delete from user_integration where user_id = $userId and kind = $kind".update.run)

  // ---------------------------------------------------------------- アドレス帳

  /** アドレス帳（saved なら自分で保存したものだけ、そうでなければ自動で覚えたものも）。 */
  def addressBook(userId: UUID, q: String, savedOnly: Boolean, limit: Int): IO[List[AddressBookDto]] = tx {
    val p = s"%${Like.escape(q.trim.toLowerCase)}%"
    (fr"""select address, name, company, phone, note, saved, uses, last_used from contact
          where user_id = $userId and (address like $p or lower(coalesce(name, '')) like $p or lower(coalesce(company, '')) like $p)""" ++
      (if savedOnly then fr"and saved" else Fragment.empty) ++
      (if savedOnly then fr"order by lower(coalesce(name, address))" else fr"order by uses desc, last_used desc") ++ fr"limit $limit")
      .query[AddressBookDto].to[List]
  }

  def saveContact(userId: UUID, r: SaveContactReq): IO[Unit] =
    def clean(v: Option[String], max: Int) = v.map(_.trim.filterNot(_ < 0x20).take(max)).filter(_.nonEmpty)
    tx(sql"""insert into contact (user_id, address, name, company, phone, note, saved, uses)
            values ($userId, ${r.address.trim.toLowerCase}, ${clean(r.name, 200)}, ${clean(r.company, 200)}, ${clean(r.phone, 50)}, ${r.note.map(_.take(2000)).filter(_.trim.nonEmpty)}, true, 0)
            on conflict (user_id, address) do update set name = excluded.name, company = excluded.company, phone = excluded.phone,
              note = excluded.note, saved = true""".update.run.void)

  def deleteContact(userId: UUID, address: String): IO[Int] =
    tx(sql"delete from contact where user_id = $userId and address = ${address.trim.toLowerCase}".update.run)

/** LIKE の特殊文字のエスケープ。 */
object Like:
  def escape(s: String): String = s.replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_")
