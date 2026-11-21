package skereru.shared

import io.circe.*

import java.util.UUID

// バックエンド(JVM)と画面(Scala.js)の両方で使う API の型。
// 日時は Timestamp（JVM では Instant）、日付は "yyyy-MM-dd" の文字列（Scala.js で java.time を使わないため）。

/** 特別な用途のメールボックス（IMAP の SPECIAL-USE）。 */
object SpecialUse {
  val Inbox = "inbox"
  val Drafts = "drafts"
  val Sent = "sent"
  val Junk = "junk"
  val Trash = "trash"
  val Archive = "archive"
  val all: List[String] = List(Inbox, Drafts, Sent, Junk, Trash, Archive)

  /** 新しいユーザーに作るメールボックスの名前. */
  def defaultName(u: String): String = u match {
    case Inbox => "INBOX"
    case Drafts => "Drafts"
    case Sent => "Sent"
    case Junk => "Junk"
    case Trash => "Trash"
    case Archive => "Archive"
    case other => other
  }
  def label(u: String): String = u match {
    case Inbox => "受信トレイ"
    case Drafts => "下書き"
    case Sent => "送信済み"
    case Junk => "迷惑メール"
    case Trash => "ゴミ箱"
    case Archive => "アーカイブ"
    case other => other
  }
  def icon(u: String): String = u match {
    case Inbox => "📥"
    case Drafts => "📝"
    case Sent => "📤"
    case Junk => "🚫"
    case Trash => "🗑"
    case Archive => "🗄"
    case _ => "📁"
  }

  /** IMAP の属性（\Sent など）. */
  def imapAttr(u: String): Option[String] = u match {
    case Drafts => Some("\\Drafts")
    case Sent => Some("\\Sent")
    case Junk => Some("\\Junk")
    case Trash => Some("\\Trash")
    case Archive => Some("\\Archive")
    case _ => None
  }
}

/** メッセージのフラグ（IMAP のシステムフラグ）. */
object Flag {
  val Seen = "\\Seen"
  val Answered = "\\Answered"
  val Flagged = "\\Flagged"
  val Deleted = "\\Deleted"
  val Draft = "\\Draft"
  val Forwarded = "$Forwarded"
  val Junk = "$Junk"
  val NotJunk = "$NotJunk"
  val system: List[String] = List(Seen, Answered, Flagged, Deleted, Draft)
}

/** APIキーのスコープ. */
object ApiScope {
  val Read = "read"
  val Write = "write"
  val Send = "send"
  val Admin = "admin"
  val all: List[String] = List(Read, Write, Send, Admin)
  def label(s: String): String = s match {
    case Read => "参照"
    case Write => "整理（既読・移動・削除・設定）"
    case Send => "送信"
    case Admin => "管理"
    case _ => s
  }
}

/** 振り分けの規則の条件と処理. */
object RuleField {
  val From = "from"
  val To = "to"
  val Cc = "cc"
  val Recipient = "recipient"
  val Subject = "subject"
  val Body = "body"
  val ListId = "list-id"
  val Header = "header"
  val all: List[String] = List(From, To, Cc, Recipient, Subject, Body, ListId, Header)
  def label(f: String): String = f match {
    case From => "差出人"
    case To => "宛先（To）"
    case Cc => "Cc"
    case Recipient => "宛先（To・Cc）"
    case Subject => "件名"
    case Body => "本文"
    case ListId => "メーリングリスト（List-Id）"
    case Header => "ヘッダー"
    case _ => f
  }
}

object RuleOp {
  val Contains = "contains"
  val NotContains = "not_contains"
  val Equals = "equals"
  val StartsWith = "starts_with"
  val EndsWith = "ends_with"
  val all: List[String] = List(Contains, NotContains, Equals, StartsWith, EndsWith)
  def label(o: String): String = o match {
    case Contains => "を含む"
    case NotContains => "を含まない"
    case Equals => "と一致する"
    case StartsWith => "で始まる"
    case EndsWith => "で終わる"
    case _ => o
  }
}

object RuleAction {
  val Move = "move"
  val MarkRead = "mark_read"
  val Flag = "flag"
  val Delete = "delete"
  val Forward = "forward"
  val NotSpam = "not_spam"
  val Stop = "stop"

  /** 通知先（stui・Webhook）に知らせる. arg は通知先の ID. */
  val Notify = "notify"

  /** 添付ファイルを skawau（ファイル共有）の「メールの添付」に保存する. */
  val SaveSkawau = "save_skawau"
  val all: List[String] = List(Move, MarkRead, Flag, Delete, Forward, NotSpam, Notify, SaveSkawau, Stop)
  def label(a: String): String = a match {
    case Move => "メールボックスへ移動"
    case MarkRead => "既読にする"
    case Flag => "スターを付ける"
    case Delete => "ゴミ箱へ移動"
    case Forward => "転送する"
    case NotSpam => "迷惑メールにしない"
    case Stop => "以降の規則を適用しない"
    case Notify => "通知先に知らせる"
    case SaveSkawau => "添付を skawau に保存"
    case _ => a
  }
  def needsArg(a: String): Boolean = a == Move || a == Forward || a == Notify
}

// ---------------------------------------------------------------- 自分・アドレス

final case class AddrDto(name: Option[String], address: String) derives Encoder.AsObject, Decoder

/** 宛先の書き方（"名前" <addr>。名前の引用符・バックスラッシュは取り除く）とメールボックスの名前の検査. 画面とサーバーで共通. */
object MailText {
  def quoted(name: Option[String], address: String): String =
    name.map(_.replace("\"", "").replace("\\", "").trim).filter(_.nonEmpty).fold(address)(n =>
      "\"" + n + "\" <" + address + ">"
    )

  /** メールボックスの名前（階層は /。空・前後の /・// ・制御文字・IMAP のワイルドカードは不可、INBOX は予約）. */
  def validMailboxName(n: String): Boolean =
    n.nonEmpty && n.length <= 200 && !n.startsWith("/") && !n.endsWith("/") && !n.contains("//") &&
      !n.exists(c => c < 0x20 || c == '*' || c == '%') && !n.equalsIgnoreCase("INBOX")
}

final case class AddressDto(id: UUID,
                            address: String,
                            domainId: UUID,
                            userId: Option[UUID],
                            userName: Option[String],
                            forwardTo: List[String],
                            isPrimary: Boolean,
                            canSend: Boolean) derives Encoder.AsObject, Decoder

final case class MeDto(id: UUID,
                       name: String,
                       email: Option[String],
                       avatarUrl: Option[String],
                       isAdmin: Boolean,
                       timezone: String,
                       addresses: List[AddressDto],
                       quotaBytes: Long,
                       usedBytes: Long,
                       via: String,
                       scopes: List[String],
                       appNames: Map[String, String] = Map.empty) derives Encoder.AsObject, Decoder

final case class UpdateMeReq(name: Option[String], timezone: Option[String]) derives Encoder.AsObject, Decoder

final case class VacationDto(enabled: Boolean,
                             subject: String,
                             body: String,
                             startDate: Option[String],
                             endDate: Option[String],
                             contactsOnly: Boolean) derives Encoder.AsObject, Decoder

object VacationDto {
  val empty: VacationDto = VacationDto(false, "不在のお知らせ", "", None, None, false)
}

/** 個人の設定（署名・自動返信・画像の表示・許可/拒否する差出人）. */
/** undoSendSeconds: 送信を取り消せる時間（0 ならすぐ送る）. */
final case class MailSettingsDto(signature: String,
                                 vacation: VacationDto,
                                 alwaysShowImages: Boolean,
                                 allowSenders: List[String],
                                 blockSenders: List[String],
                                 conversationView: Boolean,
                                 pageSize: Int,
                                 undoSendSeconds: Int = 10) derives Encoder.AsObject, Decoder

object MailSettingsDto {
  val default: MailSettingsDto = MailSettingsDto("", VacationDto.empty, false, Nil, Nil, true, 50)
}

final case class ContactDto(address: String, name: Option[String], count: Int) derives Encoder.AsObject, Decoder

/** アドレス帳の1件. saved: 自分で保存したもの（false は宛先として使ったので自動で覚えたもの）. */
final case class AddressBookDto(address: String,
                                name: Option[String],
                                company: Option[String],
                                phone: Option[String],
                                note: Option[String],
                                saved: Boolean,
                                uses: Int,
                                lastUsed: Timestamp) derives Encoder.AsObject, Decoder

/** 連絡先の保存（同じアドレスなら上書き）. */
final case class SaveContactReq(address: String,
                                name: Option[String],
                                company: Option[String],
                                phone: Option[String],
                                note: Option[String])
  derives Encoder.AsObject, Decoder

// ---------------------------------------------------------------- メールボックス・メッセージ

final case class MailboxDto(id: UUID,
                            name: String,
                            specialUse: Option[String],
                            total: Int,
                            unread: Int,
                            subscribed: Boolean)
  derives Encoder.AsObject, Decoder

final case class CreateMailboxReq(name: String) derives Encoder.AsObject, Decoder
final case class RenameMailboxReq(name: String) derives Encoder.AsObject, Decoder

final case class MessageSummaryDto(id: UUID,
                                   mailboxId: UUID,
                                   threadId: UUID,
                                   subject: String,
                                   from: Option[AddrDto],
                                   to: List[AddrDto],
                                   date: Timestamp,
                                   preview: String,
                                   seen: Boolean,
                                   flagged: Boolean,
                                   answered: Boolean,
                                   draft: Boolean,
                                   hasAttachments: Boolean,
                                   size: Int,
                                   threadCount: Int,
                                   spamScore: Option[Double]) derives Encoder.AsObject, Decoder

final case class MessageListDto(items: List[MessageSummaryDto], total: Int, offset: Int) derives Encoder.AsObject,
    Decoder

final case class AttachmentDto(part: String,
                               filename: String,
                               contentType: String,
                               size: Int,
                               inline: Boolean,
                               contentId: Option[String])
  derives Encoder.AsObject, Decoder

final case class AuthResultDto(spf: String, dkim: String, dmarc: String) derives Encoder.AsObject, Decoder

final case class MessageDetailDto(summary: MessageSummaryDto,
                                  cc: List[AddrDto],
                                  bcc: List[AddrDto],
                                  replyTo: List[AddrDto],
                                  messageIdHeader: Option[String],
                                  references: List[String],
                                  html: Option[String],
                                  text: String,
                                  hasRemoteImages: Boolean,
                                  attachments: List[AttachmentDto],
                                  spamReport: Option[String],
                                  auth: Option[AuthResultDto],
                                  listUnsubscribe: Option[String],
                                  mailboxName: String,
                                  hasInvite: Boolean = false) derives Encoder.AsObject, Decoder

final case class ThreadDto(threadId: UUID, subject: String, messages: List[MessageDetailDto]) derives Encoder.AsObject,
    Decoder

/** まとめての変更（既読・スター・移動・迷惑メールの報告・削除）. */
final case class UpdateMessagesReq(ids: List[UUID],
                                   seen: Option[Boolean],
                                   flagged: Option[Boolean],
                                   moveTo: Option[UUID],
                                   spam: Option[Boolean],
                                   delete: Option[Boolean]) derives Encoder.AsObject, Decoder

object UpdateMessagesReq {

  /** 何も変えない要求（copy で変えたいものだけ指定する）. */
  def of(ids: List[UUID]): UpdateMessagesReq = UpdateMessagesReq(ids, None, None, None, None, None)
}

final case class UpdatedDto(count: Int) derives Encoder.AsObject, Decoder

final case class UploadDto(id: UUID, filename: String, contentType: String, size: Long) derives Encoder.AsObject,
    Decoder

/** メールを書く（送信・下書きの保存）. replyTo / forwardOf は元のメッセージ. */
final case class ComposeReq(from: String,
                            to: List[String],
                            cc: List[String],
                            bcc: List[String],
                            subject: String,
                            text: String,
                            html: Option[String],
                            uploadIds: List[UUID],
                            inReplyTo: Option[UUID],
                            forwardOf: Option[UUID],
                            draftId: Option[UUID])
  derives Encoder.AsObject, Decoder

final case class SentDto(messageId: UUID, queued: Int, delivered: Int) derives Encoder.AsObject, Decoder

/** 予約送信（sendAt がなければ、取り消しの猶予の後に送る）. */
final case class ScheduleReq(req: ComposeReq, sendAt: Option[Timestamp]) derives Encoder.AsObject, Decoder

/** 予約した送信. status: queued | sending | sent | failed | cancelled. */
final case class ScheduledDto(id: UUID,
                              sendAt: Timestamp,
                              subject: String,
                              to: List[String],
                              status: String,
                              error: Option[String])
  derives Encoder.AsObject, Decoder

/** 取り消した予約（作成画面に戻す内容と、その添付）. */
final case class ScheduledCancelDto(req: ComposeReq, uploads: List[UploadDto]) derives Encoder.AsObject, Decoder

/** スヌーズ（until まで一覧に出さない）. */
final case class SnoozeReq(ids: List[UUID], until: Option[Timestamp]) derives Encoder.AsObject, Decoder
final case class DraftSavedDto(draftId: UUID) derives Encoder.AsObject, Decoder

/** 返信・転送の下書きの初期値. */
final case class ComposeTemplateDto(from: String,
                                    to: List[String],
                                    cc: List[String],
                                    subject: String,
                                    quotedText: String,
                                    quotedHtml: String,
                                    attachments: List[AttachmentDto]) derives Encoder.AsObject, Decoder

// ---------------------------------------------------------------- 振り分け・パスワード・APIキー

final case class RuleConditionDto(field: String, op: String, value: String, header: Option[String])
  derives Encoder.AsObject, Decoder
final case class RuleActionDto(kind: String, arg: Option[String]) derives Encoder.AsObject, Decoder

final case class FilterRuleDto(id: UUID,
                               name: String,
                               enabled: Boolean,
                               matchAll: Boolean,
                               conditions: List[RuleConditionDto],
                               actions: List[RuleActionDto],
                               position: Int) derives Encoder.AsObject, Decoder

final case class FilterRuleReq(name: String,
                               enabled: Boolean,
                               matchAll: Boolean,
                               conditions: List[RuleConditionDto],
                               actions: List[RuleActionDto])
  derives Encoder.AsObject, Decoder

final case class ReorderReq(ids: List[UUID]) derives Encoder.AsObject, Decoder

final case class AppPasswordDto(id: UUID,
                                name: String,
                                prefix: String,
                                createdAt: Timestamp,
                                lastUsedAt: Option[Timestamp],
                                lastUsedIp: Option[String])
  derives Encoder.AsObject, Decoder
final case class CreateAppPasswordReq(name: String) derives Encoder.AsObject, Decoder
final case class CreatedAppPasswordDto(password: String, username: String, info: AppPasswordDto)
  derives Encoder.AsObject, Decoder

final case class ApiKeyDto(id: UUID,
                           name: String,
                           prefix: String,
                           scopes: List[String],
                           createdAt: Timestamp,
                           expiresAt: Option[Timestamp],
                           lastUsedAt: Option[Timestamp]) derives Encoder.AsObject, Decoder
final case class CreateApiKeyReq(name: String, scopes: List[String], expiresDays: Option[Int]) derives Encoder.AsObject,
    Decoder

/** メールの取り込み（Gmail など）。kind: imap | file。status: queued | running | done | failed | cancelled. */
final case class ImportJobDto(id: UUID,
                              kind: String,
                              status: String,
                              source: String,
                              imported: Int,
                              skipped: Int,
                              failed: Int,
                              current: String,
                              error: Option[String],
                              createdAt: Timestamp,
                              finishedAt: Option[Timestamp]) derives Encoder.AsObject, Decoder

/** IMAP での取り込みを始める（Gmail なら imap.gmail.com:993 とアプリ パスワード）. */
final case class ImapImportReq(host: String, port: Int, user: String, password: String) derives Encoder.AsObject,
    Decoder
final case class FileImportReq(name: String) derives Encoder.AsObject, Decoder

/** ファイルの取り込みで、1回に送ったメッセージの結果. */
final case class ImportBatchDto(imported: Int, skipped: Int, failed: Int) derives Encoder.AsObject, Decoder
final case class CreatedApiKeyDto(key: String, info: ApiKeyDto) derives Encoder.AsObject, Decoder

// ---------------------------------------------------------------- 管理

final case class DnsRecordDto(kind: String,
                              name: String,
                              value: String,
                              purpose: String,
                              ok: Option[Boolean],
                              actual: Option[String])
  derives Encoder.AsObject, Decoder

final case class DomainDto(id: UUID,
                           name: String,
                           enabled: Boolean,
                           autoProvision: Boolean,
                           catchAll: Option[UUID],
                           dkimSelector: String,
                           addressCount: Int,
                           createdAt: Timestamp) derives Encoder.AsObject, Decoder

final case class CreateDomainReq(name: String, autoProvision: Boolean) derives Encoder.AsObject, Decoder
final case class UpdateDomainReq(enabled: Option[Boolean],
                                 autoProvision: Option[Boolean],
                                 catchAll: Option[Option[UUID]])
  derives Encoder.AsObject, Decoder

final case class AdminUserDto(id: UUID,
                              name: String,
                              email: Option[String],
                              isAdmin: Boolean,
                              disabled: Boolean,
                              linked: Boolean,
                              addresses: List[String],
                              usedBytes: Long,
                              quotaBytes: Long,
                              lastLoginAt: Option[Timestamp],
                              createdAt: Timestamp) derives Encoder.AsObject, Decoder

/** メールのユーザーを作る（IdP のメールアドレス email で最初にログインした人に結び付く）. */
final case class CreateUserReq(name: String, address: String, email: Option[String], quotaMb: Option[Int])
  derives Encoder.AsObject, Decoder
final case class UpdateUserReq(name: Option[String],
                               isAdmin: Option[Boolean],
                               disabled: Option[Boolean],
                               quotaMb: Option[Int])
  derives Encoder.AsObject, Decoder

/** エージェント専用アカウント（skakapo などの外の AI エージェントが APIキーで使う。ログインはできない）. */
final case class AgentDto(id: UUID,
                          name: String,
                          disabled: Boolean,
                          createdAt: Timestamp,
                          addresses: List[String],
                          keys: List[ApiKeyDto])
  derives Encoder.AsObject, Decoder

/** address を指定すると、そのアドレス（送信できる主アドレス）も作る. */
final case class CreateAgentReq(name: String, address: Option[String]) derives Encoder.AsObject, Decoder
final case class UpdateAgentReq(name: Option[String], disabled: Option[Boolean]) derives Encoder.AsObject, Decoder

final case class AddressReq(address: String, userId: Option[UUID], forwardTo: List[String], canSend: Boolean)
  derives Encoder.AsObject, Decoder

final case class QueueItemDto(id: UUID,
                              sender: String,
                              recipient: String,
                              status: String,
                              attempts: Int,
                              nextAttemptAt: Timestamp,
                              lastError: Option[String],
                              createdAt: Timestamp) derives Encoder.AsObject, Decoder

final case class AuditDto(id: Long,
                          at: Timestamp,
                          user: Option[String],
                          via: String,
                          action: String,
                          target: Option[String],
                          ip: Option[String],
                          detail: Json) derives Encoder.AsObject, Decoder

final case class StatsDto(users: Int,
                          domains: Int,
                          messages: Long,
                          bytes: Long,
                          queued: Int,
                          failed: Int,
                          received24h: Int,
                          junk24h: Int,
                          rejected24h: Int,
                          sent24h: Int) derives Encoder.AsObject, Decoder

/** 画面の初期化に使う設定（メールソフトの設定の案内を含む）. */
final case class ClientConfigDto(devLogin: Boolean,
                                 version: String,
                                 hostname: String,
                                 imapPort: Option[Int],
                                 imapsPort: Option[Int],
                                 pop3Port: Option[Int],
                                 pop3sPort: Option[Int],
                                 submissionPort: Option[Int],
                                 smtpsPort: Option[Int],
                                 maxMessageMb: Int)
  derives Encoder.AsObject, Decoder

/** DMARC の集計レポート（一覧）. passing は DKIM か SPF が pass の通数. */
final case class DmarcReportDto(id: UUID,
                                domain: String,
                                orgName: String,
                                reportId: String,
                                policy: String,
                                begin: Timestamp,
                                end: Timestamp,
                                total: Long,
                                passing: Long,
                                authenticated: Boolean,
                                receivedAt: Timestamp) derives Encoder.AsObject, Decoder

/** DMARC の集計レポートの1行（送信元の IP ごと）. */
final case class DmarcRowDto(sourceIp: String,
                             count: Long,
                             disposition: String,
                             dkim: String,
                             spf: String,
                             headerFrom: String,
                             envelopeFrom: Option[String],
                             dkimDomains: List[String],
                             spfDomains: List[String]) derives Encoder.AsObject, Decoder

/** 会議の招待（メールの中の iCalendar）. myStatus は自分の返事（ACCEPTED・TENTATIVE・DECLINED・NEEDS-ACTION）. */
final case class InviteAttendeeDto(address: String, name: Option[String], partstat: Option[String])
  derives Encoder.AsObject, Decoder
final case class InviteDto(method: String,
                           summary: String,
                           description: String,
                           location: String,
                           allDay: Boolean,
                           start: Timestamp,
                           end: Timestamp,
                           organizer: Option[AddrDto],
                           attendees: List[InviteAttendeeDto],
                           me: Option[String],
                           myStatus: Option[String],
                           recurring: Boolean,
                           canReply: Boolean,
                           canAddToCalendar: Boolean) derives Encoder.AsObject, Decoder
final case class InviteReplyReq(partstat: String) derives Encoder.AsObject, Decoder
final case class CalendarAddedDto(url: String) derives Encoder.AsObject, Decoder

/** 兄弟のサービスとの連携の状態. skuakaUrl はサーバーに設定された skuaka（なければ連携できない）. */
final case class IntegrationsDto(skuakaUrl: Option[String], skuakaConnected: Boolean) derives Encoder.AsObject, Decoder
final case class TokenReq(token: String) derives Encoder.AsObject, Decoder

// ---------------------------------------------------------------- 通知先（stui・Webhook）

/** 通知先の種類. */
object NotifyKind {

  /** stui（OAuth で連携した本人のトークンで、自分への DM か、参加しているチャンネルへ投稿する）. */
  val Stui = "stui"

  /** stui・Slack・Mattermost の受信 Webhook（{"text": …} を送る）. */
  val ChatHook = "chat_hook"

  /** 署名（HMAC-SHA256）付きの JSON を送る Webhook. */
  val Webhook = "webhook"
  val all: List[String] = List(Stui, ChatHook, Webhook)
  def label(k: String): String = k match {
    case Stui => "stui（連携）"
    case ChatHook => "受信 Webhook（stui・Slack・Mattermost）"
    case Webhook => "Webhook（署名付きの JSON）"
    case _ => k
  }
}

/**
 * 通知先. allInbox なら受信トレイの新着をすべて知らせる（振り分けの規則の「通知先に知らせる」でも選べる）.
 * stui: channelId が None なら自分への DM（channelName は表示用）. chat_hook・webhook: url に送る. secret は webhook の署名の鍵.
 * lastError は、最後に送れなかったときの理由.
 */
final case class NotifyTargetDto(id: UUID,
                                 name: String,
                                 kind: String,
                                 channelId: Option[String],
                                 channelName: Option[String],
                                 url: Option[String],
                                 secret: Option[String],
                                 allInbox: Boolean,
                                 enabled: Boolean,
                                 lastError: Option[String],
                                 createdAt: Timestamp) derives Encoder.AsObject, Decoder

final case class NotifyTargetReq(name: String,
                                 kind: String,
                                 channelId: Option[String],
                                 channelName: Option[String],
                                 url: Option[String],
                                 allInbox: Boolean,
                                 enabled: Boolean) derives Encoder.AsObject, Decoder

/**
 * 兄弟のサービス（stui・skawau）との連携の状態. configured: 管理者が設定したか. name: sruru に登録した表示名.
 * lastError: 最後に失敗した裏の処理の理由（skawau への自動の保存など）.
 */
final case class LinkStatusDto(configured: Boolean,
                               connected: Boolean,
                               url: Option[String],
                               name: String,
                               lastError: Option[String] = None)
  derives Encoder.AsObject, Decoder

// ---------------------------------------------------------------- skawau（ファイル共有）

/** skawau のファイル・フォルダ（選ぶ画面に出す分だけ）. */
final case class SkawauItemDto(id: UUID, name: String, isDir: Boolean, size: Long, contentType: Option[String])
  derives Encoder.AsObject, Decoder

/** skawau のフォルダの中身. path はルートからのパンくず（id・名前）. */
final case class SkawauFolderDto(id: UUID, name: String, path: List[SkawauItemDto], items: List[SkawauItemDto])
  derives Encoder.AsObject, Decoder

/** skawau のファイルをメールに付ける（添付にする）. */
final case class SkawauAttachReq(ids: List[UUID]) derives Encoder.AsObject, Decoder

/** skawau のファイル・フォルダの URL（本文にリンクとして入れる）. */
final case class SkawauLinkDto(id: UUID, name: String, url: String, isDir: Boolean) derives Encoder.AsObject, Decoder

/** メールの添付を skawau に保存する. parts が空ならすべての添付. folderId がなければ「メールの添付」フォルダ. */
final case class SkawauSaveReq(messageId: UUID, parts: List[String], folderId: Option[UUID])
  derives Encoder.AsObject, Decoder

/** 保存したファイル（名前は、同じ名前があって変えたときは変えた後の名前）. */
final case class SkawauSavedDto(saved: List[SkawauLinkDto], folderUrl: Option[String]) derives Encoder.AsObject, Decoder

/** 通知先に選べる stui のチャンネル. */
final case class StuiChannelDto(id: String, name: String, kind: String) derives Encoder.AsObject, Decoder

/** 利用者が OAuth で許可した外部のサービス（skakapo など）。設定の画面で確認・取り消しができる. */
final case class OAuthGrantDto(clientId: String,
                               name: String,
                               scopes: List[String],
                               createdAt: Timestamp,
                               lastUsedAt: Option[Timestamp])
  derives Encoder.AsObject, Decoder
