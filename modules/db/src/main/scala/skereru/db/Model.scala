package skereru.db

import io.circe.Json

import java.time.Instant
import java.util.UUID

final case class User(
    id: UUID,
    oidcSub: Option[String],
    name: String,
    email: Option[String],
    avatarUrl: Option[String],
    isAdmin: Boolean,
    disabled: Boolean,
    timezone: String,
    quotaBytes: Option[Long],
    usedBytes: Long,
    createdAt: Instant,
    lastLoginAt: Option[Instant]
)

final case class Domain(
    id: UUID,
    name: String,
    enabled: Boolean,
    autoProvision: Boolean,
    catchAll: Option[UUID],
    dkimSelector: String,
    dkimPrivate: String,
    dkimPublic: String,
    createdAt: Instant
)

final case class AddressRow(
    id: UUID,
    address: String,
    domainId: UUID,
    userId: Option[UUID],
    userName: Option[String],
    forwardTo: List[String],
    isPrimary: Boolean,
    canSend: Boolean
)

final case class Mailbox(
    id: UUID,
    userId: UUID,
    name: String,
    specialUse: Option[String],
    uidvalidity: Long,
    uidnext: Long,
    highestModseq: Long,
    subscribed: Boolean
)

/** メッセージの属性（本体は blob）。 */
final case class MsgMeta(
    id: UUID,
    userId: UUID,
    mailboxId: UUID,
    uid: Long,
    modseq: Long,
    blobId: String,
    size: Int,
    internalDate: Instant,
    flags: List[String],
    subject: String,
    fromAddr: Option[String],
    fromName: Option[String],
    toAddrs: String,
    ccAddrs: String,
    sentAt: Option[Instant],
    messageId: Option[String],
    inReplyTo: Option[String],
    threadId: UUID,
    hasAttachments: Boolean,
    preview: String,
    spamScore: Option[Double],
    spamReport: Option[String],
    authResults: Option[Json]
)

/** 保存するメッセージ（解析済みの属性）。 */
final case class NewMessage(
    blobId: String,
    size: Int,
    internalDate: Instant,
    flags: List[String],
    subject: String,
    fromAddr: Option[String],
    fromName: Option[String],
    toAddrs: String,
    ccAddrs: String,
    sentAt: Option[Instant],
    messageId: Option[String],
    inReplyTo: Option[String],
    references: List[String],
    hasAttachments: Boolean,
    preview: String,
    searchText: String,
    spamScore: Option[Double],
    spamReport: Option[String],
    authResults: Option[Json]
)

/** IMAP のセッションが変化を知るための軽い行。 */
final case class UidFlags(uid: Long, flags: List[String], modseq: Long)

final case class Outbound(
    id: UUID,
    userId: Option[UUID],
    sender: String,
    recipient: String,
    domain: String,
    blobId: String,
    status: String,
    attempts: Int,
    nextAttemptAt: Instant,
    lastError: Option[String],
    createdAt: Instant
)

final case class Upload(id: UUID, userId: UUID, filename: String, contentType: String, size: Long, blobId: String)

/** 認証済みのユーザーとその手段。 */
enum AuthVia:
  case Session
  case ApiKey(keyId: UUID, scopes: Set[String], mcp: Boolean)
  case AppPassword(id: UUID)
  case Dev

final case class AuthSession(user: User, groups: List[String], isAdmin: Boolean, via: AuthVia = AuthVia.Session):
  def isSession: Boolean = via == AuthVia.Session || via == AuthVia.Dev
  def viaString: String = via match
    case AuthVia.Session | AuthVia.Dev  => "web"
    case AuthVia.ApiKey(_, _, true)     => "mcp"
    case AuthVia.ApiKey(_, _, false)    => "api"
    case AuthVia.AppPassword(_)         => "mail"
  def scopes: Set[String] = via match
    case AuthVia.ApiKey(_, s, _) => s
    case _                       => Set("read", "write", "send") ++ (if isAdmin then Set("admin") else Set.empty)
  def can(scope: String): Boolean = scopes.contains(scope) && (scope != "admin" || isAdmin)

/** エージェント専用アカウント（skakapo などの外の AI エージェントが APIキーで使う。ログインはできない）。
  * oidc_sub を `agent:<uuid>` にし、email は持たない（IdP の sub・開発用のログインの `dev:` と重ならず、email での結び付けもされない）。 */
object AgentAccounts:
  val Prefix = "agent:"
  def isAgent(sub: String): Boolean = sub.startsWith(Prefix)
  def isAgent(u: User): Boolean = u.oidcSub.exists(isAgent)
  def newSub(): String = Prefix + UUID.randomUUID()

  /** 名前を確かめる（前後の空白を除いて1〜60文字・制御文字なし）。 */
  def validName(name: String): Either[String, String] =
    val n = name.trim
    if n.isEmpty || n.length > 60 then Left("名前は1〜60文字で入力してください")
    else if n.exists(Character.isISOControl) then Left("名前に制御文字は使えません")
    else Right(n)
