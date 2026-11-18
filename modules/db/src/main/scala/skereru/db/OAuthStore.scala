package skereru.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skereru.shared.*

import java.time.Instant
import java.util.UUID

final case class OAuthCode(clientId: String, redirectUri: String, userId: UUID, codeChallenge: String, scope: String)

/** 許可（連携）。refresh token で新しいアクセストークンを発行するときに使う。 */
final case class OAuthGrant(id: UUID, userId: UUID, clientId: String, scopes: List[String])

/** OAuth の提供側: 認可コード・許可（refresh token）・許可で発行したアクセストークン（有効期限つきの API キー）。 */
final class OAuthStore(xa: Transactor[IO]) extends TxStore(xa):

  def createCode(hash: Array[Byte], c: OAuthCode, expires: Instant): IO[Unit] = tx(
    sql"""insert into oauth_code (code_hash, client_id, redirect_uri, user_id, code_challenge, scope, expires_at)
          values ($hash, ${c.clientId}, ${c.redirectUri}, ${c.userId}, ${c.codeChallenge}, ${c.scope}, $expires)""".update.run *>
      sql"delete from oauth_code where expires_at < now() - interval '1 hour'".update.run.void
  )

  /** 認可コードを使う（有効期限内で、まだ使っていないものだけ。使ったら使用済みにする）。 */
  def consumeCode(hash: Array[Byte]): IO[Option[OAuthCode]] = tx(
    sql"""update oauth_code set used_at = now() where code_hash = $hash and used_at is null and expires_at > now()
          returning client_id, redirect_uri, user_id, code_challenge, scope""".query[OAuthCode].option
  )

  private def insertKey(grant: UUID, userId: UUID, name: String, prefix: String, keyHash: Array[Byte], scopes: List[String], expires: Instant) =
    sql"""insert into api_key (id, user_id, name, key_hash, prefix, scopes, expires_at, oauth_grant)
          values (${UUID.randomUUID()}, $userId, $name, $keyHash, $prefix, $scopes, $expires, $grant)""".update.run

  /** 許可を作る（そのユーザーがそのクライアントに出していた前の許可と、そのトークンは消す）。 */
  def grant(userId: UUID, clientId: String, scopes: List[String], refreshHash: Array[Byte],
      keyName: String, prefix: String, keyHash: Array[Byte], expires: Instant): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"delete from oauth_grant where user_id = $userId and client_id = $clientId".update.run *>
      sql"insert into oauth_grant (id, user_id, client_id, scopes, refresh_hash) values ($id, $userId, $clientId, $scopes, $refreshHash)".update.run *>
      insertKey(id, userId, keyName, prefix, keyHash, scopes, expires).as(id)
  }

  /**
   * refresh token で新しいアクセストークンを発行する。refresh token も新しくする（前のものは使えなくなる）。
   * scopes は今の許可のスコープを受け取り、発行するスコープを返す（クライアントの設定が狭まったときに絞る）。
   * 期限の切れたアクセストークンはここで消す。無効にされたユーザーの許可は使えない。
   */
  def refresh(clientId: String, oldHash: Array[Byte], newHash: Array[Byte], keyName: String, prefix: String, keyHash: Array[Byte], expires: Instant)(
      scopes: List[String] => List[String]): IO[Option[OAuthGrant]] = tx(
    sql"""select g.id, g.user_id, g.client_id, g.scopes from oauth_grant g join app_user u on u.id = g.user_id
          where g.refresh_hash = $oldHash and g.client_id = $clientId and not u.disabled for update of g""".query[OAuthGrant].option.flatMap(_.traverse { g =>
      val sc = scopes(g.scopes)
      sql"update oauth_grant set refresh_hash = $newHash, scopes = $sc, refreshed_at = now() where id = ${g.id}".update.run *>
        sql"delete from api_key where oauth_grant = ${g.id} and expires_at < now()".update.run *>
        insertKey(g.id, g.userId, keyName, prefix, keyHash, sc, expires).as(g.copy(scopes = sc))
    })
  )

  /** そのクライアントの許可を取り消す（トークンは access token・refresh token のどちらでもよい。なければ何もしない）。 */
  def revokeByToken(hash: Array[Byte], clientId: String): IO[Unit] = tx(
    sql"""delete from oauth_grant where client_id = $clientId and (refresh_hash = $hash or
          id = (select oauth_grant from api_key where key_hash = $hash))""".update.run.void
  )

  /** 利用者の連携の一覧（クライアント・スコープ・許可した日時・最後に使った日時）。 */
  def grants(userId: UUID): IO[List[(String, List[String], Timestamp, Option[Timestamp])]] = tx(
    sql"""select g.client_id, g.scopes, g.created_at,
                 greatest(g.refreshed_at, max(k.last_used_at))
          from oauth_grant g left join api_key k on k.oauth_grant = g.id
          where g.user_id = $userId group by g.id order by g.created_at""".query[(String, List[String], Timestamp, Option[Timestamp])].to[List]
  )

  /** 利用者が連携を取り消す。消したら true。 */
  def revoke(userId: UUID, clientId: String): IO[Boolean] = tx(
    sql"delete from oauth_grant where user_id = $userId and client_id = $clientId".update.run.map(_ > 0)
  )
