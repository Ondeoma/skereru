package skereru.api

import cats.effect.*
import sbirds.auth.Crypto
import sbirds.shared.AppError
import skereru.auth.ApiKeys
import skereru.core.ErrorCode
import skereru.db.*
import skereru.shared.*

import java.time.Instant
import java.util.UUID

/** APIキーの発行（自分のキーと、管理者が発行するエージェント専用アカウントのキーで共通）。平文のキーは応答で1回だけ返す。 */
object ApiKeyIssuer {

  /** adminDenied: admin スコープを付けられないときの理由（付けられるなら None）。 */
  def issue(
    users: UserStore,
    userId: UUID,
    r: CreateApiKeyReq,
    adminDenied: Option[AppError]
  ): Res[CreatedApiKeyDto] = {
    val scopes = r.scopes.map(_.trim.toLowerCase).distinct
    val invalid: Option[AppError] =
      if r.name.trim.isEmpty then Some(AppError(ErrorCode.NameRequired))
      else if scopes.isEmpty || !scopes.forall(ApiScope.all.contains) then
        Some(AppError(ErrorCode.ApiKeyScopeInvalid))
      else if scopes.contains(ApiScope.Admin) then adminDenied
      else if r.expiresDays.exists(d => d < 1 || d > 3650) then Some(AppError(ErrorCode.ApiKeyExpiryInvalid))
      else None
    invalid match {
      case Some(f) => IO.pure(Left(f))
      case None =>
        val (key, prefix) = ApiKeys.generate()
        users.createApiKey(
          userId,
          r.name.trim.take(100),
          Crypto.sha256(key),
          prefix,
          scopes,
          r.expiresDays.map(d => Instant.now().plusSeconds(d.toLong * 86400))
        ).map(info => Right(CreatedApiKeyDto(key, info)))
    }
  }
}
