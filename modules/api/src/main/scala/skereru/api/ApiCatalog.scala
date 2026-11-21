package skereru.api

/** REST API の一覧（MCP の list_api_endpoints と GET /api/endpoints）。 */
object ApiCatalog {
  final case class Endpoint(method: String, path: String, description: String)
  val all: List[Endpoint] = List(
    Endpoint("GET", "/api/me", "自分の情報（アドレス・容量・スコープ）"),
    Endpoint("GET", "/api/me/settings", "個人の設定（署名・自動返信・画像の表示・許可/拒否する差出人）"),
    Endpoint("PUT", "/api/me/settings", "個人の設定を変える（MailSettingsDto 全体）"),
    Endpoint("GET", "/api/contacts?q=", "宛先の候補"),
    Endpoint("GET", "/api/mailboxes", "メールボックスの一覧（件数・未読）"),
    Endpoint("POST", "/api/mailboxes", "メールボックスを作る {name}（階層は / で区切る）"),
    Endpoint("PATCH", "/api/mailboxes/{mailboxId}", "メールボックスの名前を変える {name}"),
    Endpoint("DELETE", "/api/mailboxes/{mailboxId}", "メールボックスを削除"),
    Endpoint("POST", "/api/mailboxes/{mailboxId}/empty", "ゴミ箱・迷惑メールを空にする"),
    Endpoint(
      "GET",
      "/api/messages?mailboxId=&q=&offset=&limit=",
      "メッセージの一覧・検索（q: from: to: subject: has:attachment is:unread is:starred in: before: after: larger: smaller: と自由な語）"
    ),
    Endpoint("GET", "/api/messages/{messageId}", "メッセージの詳細（本文・添付の一覧・送信ドメイン認証の結果）"),
    Endpoint("GET", "/api/messages/{messageId}/thread?markRead=true", "スレッド（会話）"),
    Endpoint("GET", "/api/messages/{messageId}/raw", "メッセージの元のデータ（.eml）"),
    Endpoint("GET", "/api/messages/{messageId}/parts/{part}", "添付ファイル（part は 1.2 などの部品番号）"),
    Endpoint(
      "POST",
      "/api/messages/update",
      "まとめての変更 {ids, seen?, flagged?, moveTo?: mailboxId, spam?: true|false, delete?: true}"
    ),
    Endpoint("GET", "/api/messages/{messageId}/compose?mode=reply|replyAll|forward", "返信・転送の初期値"),
    Endpoint("POST", "/api/uploads", "添付ファイルを送る（multipart/form-data の file）"),
    Endpoint(
      "POST",
      "/api/send",
      "メールを送る（send スコープ） {from, to, cc, bcc, subject, text, html?, uploadIds, inReplyTo?, forwardOf?, draftId?}"
    ),
    Endpoint("POST", "/api/drafts", "下書きを保存（ComposeReq）"),
    Endpoint("GET", "/api/drafts/{messageId}", "下書きを開く"),
    Endpoint("GET", "/api/filters", "振り分けの規則"),
    Endpoint(
      "POST",
      "/api/filters",
      "規則を追加 {name, enabled, matchAll, conditions: [{field: from|to|cc|recipient|subject|body|list-id|header, op: contains|not_contains|equals|starts_with|ends_with, value, header?}], actions: [{kind: move|mark_read|flag|delete|forward|not_spam|notify|save_skawau|stop, arg?}]}（notify の arg は通知先の ID）"
    ),
    Endpoint("PUT", "/api/filters/{ruleId}", "規則を変える"),
    Endpoint("DELETE", "/api/filters/{ruleId}", "規則を削除"),
    Endpoint("GET", "/api/me/notify-targets", "受信の通知先（stui・受信 Webhook・署名付きの Webhook）"),
    Endpoint(
      "POST",
      "/api/me/notify-targets",
      "通知先を作る {name, kind: stui|chat_hook|webhook, channelId?（stui. なければ自分への DM）, channelName?, url?（chat_hook・webhook）, allInbox, enabled}"
    ),
    Endpoint("PUT", "/api/me/notify-targets/{targetId}", "通知先を変える（種類は変えられない）"),
    Endpoint("DELETE", "/api/me/notify-targets/{targetId}", "通知先を削除"),
    Endpoint("POST", "/api/me/notify-targets/{targetId}/test", "通知先に試しの通知を送る"),
    Endpoint("GET", "/api/integrations/{stui|skawau}", "兄弟のサービスとの連携の状態"),
    Endpoint("GET", "/api/integrations/stui/channels", "通知先に選べる stui のチャンネル"),
    Endpoint("GET", "/api/integrations/skawau/browse?folder=", "skawau のフォルダの中身（folder を省くと自分のルート）"),
    Endpoint("GET", "/api/integrations/skawau/search?q=", "skawau のファイルを名前で探す"),
    Endpoint("POST", "/api/integrations/skawau/attach", "skawau のファイルを添付にする {ids}（返ったアップロードの ID を送信の uploadIds に入れる）"),
    Endpoint("POST", "/api/integrations/skawau/links", "skawau のファイル・フォルダの URL {ids}"),
    Endpoint(
      "POST",
      "/api/integrations/skawau/save",
      "メールの添付を skawau に保存 {messageId, parts（空ならすべて）, folderId?（省くと「メールの添付」）}"
    ),
    Endpoint("GET", "/api/admin/stats", "統計（管理者）"),
    Endpoint("GET", "/api/admin/domains", "ドメインの一覧（管理者）"),
    Endpoint("POST", "/api/admin/domains", "ドメインを追加（管理者） {name, autoProvision}"),
    Endpoint("GET", "/api/admin/domains/{domainId}/dns", "DNS に設定するレコードと確認の結果（管理者）"),
    Endpoint("GET", "/api/admin/users?q=", "ユーザーの一覧（管理者）"),
    Endpoint("POST", "/api/admin/users", "ユーザーを作る（管理者） {name, address, email?, quotaMb?}"),
    Endpoint("PATCH", "/api/admin/users/{userId}", "ユーザーを変える（管理者） {name?, isAdmin?, disabled?, quotaMb?}"),
    Endpoint("GET", "/api/admin/agents", "エージェント専用アカウントの一覧とその APIキー（管理者・画面からのみ）"),
    Endpoint("POST", "/api/admin/agents", "エージェント専用アカウントを作る（管理者・画面からのみ） {name, address?}"),
    Endpoint("PATCH", "/api/admin/agents/{agentId}", "エージェント専用アカウントの名前を変える・無効にする（管理者・画面からのみ） {name?, disabled?}"),
    Endpoint(
      "POST",
      "/api/admin/agents/{agentId}/keys",
      "エージェント専用アカウントの APIキーを発行（管理者・画面からのみ。admin スコープは不可） {name, scopes, expiresDays?}"
    ),
    Endpoint("DELETE", "/api/admin/agents/{agentId}/keys/{keyId}", "エージェント専用アカウントの APIキーを失効（管理者・画面からのみ）"),
    Endpoint("GET", "/api/admin/addresses?domainId=", "アドレス・別名の一覧（管理者）"),
    Endpoint("POST", "/api/admin/addresses", "別名・転送を追加（管理者） {address, userId?, forwardTo, canSend}"),
    Endpoint("GET", "/api/admin/queue?status=queued|sent|failed", "外への送信待ち（管理者）"),
    Endpoint("GET", "/api/admin/audit", "監査ログ（管理者）"),
    Endpoint("GET", "/api/admin/dmarc?domain=", "DMARC の集計レポートの一覧（管理者）"),
    Endpoint("GET", "/api/admin/dmarc/{id}/rows", "DMARC の集計レポートの送信元ごとの行（管理者）"),
    Endpoint("POST", "/api/schedule", "日時を指定して送る（send スコープ） {req: ComposeReq, sendAt?: ISO 8601。なければ取り消しの猶予の後}"),
    Endpoint("GET", "/api/scheduled", "予約した送信の一覧"),
    Endpoint("POST", "/api/scheduled/{id}/cancel", "予約した送信を取り消す（send スコープ）。作成の内容を返す"),
    Endpoint("POST", "/api/messages/snooze", "スヌーズ {ids, until?: ISO 8601。なければ解除}"),
    Endpoint("GET", "/api/address-book?q=&all=", "アドレス帳（all=true なら自動で覚えた宛先も）"),
    Endpoint("POST", "/api/address-book", "連絡先を保存 {address, name?, company?, phone?, note?}"),
    Endpoint("GET", "/api/messages/{id}/invite", "メッセージの中の会議の招待"),
    Endpoint("POST", "/api/messages/{id}/invite/reply", "招待に返事（send スコープ） {partstat: ACCEPTED|TENTATIVE|DECLINED}")
  )
}
