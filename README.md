# skereru

<p align="center"><img src="frontend/public/logo.svg" width="128" height="128" alt="skereru のロゴ"></p>

skereru は、SMTP・IMAP・POP3 を備えたメールサーバーです。Gmail のような Web メール、HTTP API・MCP（AI との連携）を持ち、
複数ノードで冗長化できます。ライセンス: MIT

- **プロトコル**: SMTP（受信 25番、送信 587番 STARTTLS・465番 SMTPS）、IMAP4rev1（143番 STARTTLS・993番。UIDPLUS・MOVE・IDLE・SPECIAL-USE・
  NAMESPACE・ID・UNSELECT・LITERAL+・SASL-IR）、POP3（110番 STLS・995番。UIDL・TOP）
- **Web メール**: 受信トレイ・フォルダ（階層）・スレッド表示・検索（`from:` `to:` `subject:` `has:attachment` `is:unread` `is:starred` `is:snoozed` `in:`
  `before:` `after:` `larger:` と日本語の全文検索。添付ファイル（テキスト・PDF・Office・ODF）の中身も検索できる）・スター付き・スヌーズ・既読/未読・アーカイブ・移動・迷惑メールの報告・ゴミ箱、
  作成（リッチテキスト・宛先の候補・Cc/Bcc・添付（本文へのドロップも可）・下書きの自動保存・返信/全員に返信/転送・署名）、
  送信の取り消し（既定 10 秒）・予約送信、アドレス帳、会議の招待（iCalendar）の表示と返事（参加・仮承諾・辞退）・skuaka のカレンダーへの登録、
  キーボードのショートカット（`?` で一覧）、新着の通知（ブラウザの通知・タブの未読の数）、
  HTML メールの安全な表示（無害化＋sandbox の iframe、外部画像は既定で読み込まない）、ダークモード、スマートフォン対応
- **マルチドメイン**: ドメインごとの DKIM の鍵、別名・転送・グループ（複数の人に配るアドレス）・キャッチオール・サブアドレス（`user+tag@`）、
  IdP のメールアドレスでログインした人のメールボックスの自動作成、DNS に設定するレコードの案内と確認（MX・SPF・DKIM・DMARC・MTA-STS・TLS-RPT・PTR）、
  DMARC の集計レポートの受信と表示（`dmarc-reports@<ドメイン>` に届いたものを、管理画面の「DMARC」で見られる）
- **認証**: Web は OIDC（Authorization Code + PKCE）。メールソフトはアプリパスワード（ユーザーが画面で発行・失効）。API・MCP はスコープ付きの APIキー
- **アイコン**: 設定の画面で画像を選ぶと、中央を正方形に切り抜いて保存する（PNG・JPEG・GIF・WebP。消すと IdP（sruru など）の画像に戻る）
- **迷惑メールの対策**: SPF・DKIM・DMARC の検証、DNSBL、逆引き、ヘッダー・本文・添付の規則、ベイズ分類（ユーザーごと・全体で学習。
  「迷惑メール」への移動・報告で学習）、ClamAV（任意）、グレイリスト（任意）、自ドメインのなりすましの検出、点数による振り分け・受信拒否
- **振り分け・自動化**: 規則（差出人・宛先・件名・本文・List-Id・任意のヘッダー → 移動・既読・スター・ゴミ箱・転送・迷惑メールにしない・通知先に知らせる・添付を skawau に保存）、
  不在の自動返信（期間・やりとりした相手だけ・同じ相手には4日に1回）、許可/拒否する差出人
- **送信**: DKIM 署名（RSA-SHA256）、転送するメールへの ARC の封印（転送先で受信時の認証の結果を引き継ぐ）、MX への直接配送（STARTTLS・TLS の必須化・証明書の検証は設定可能）または中継サーバー経由、再送（指数的に間隔を延ばす）、
  配送できなかったときの通知（DSN）
- **取り込み**: Gmail などのメールを IMAP で取り込む（サーバーが裏で進める。Gmail はアプリ パスワード。ラベルはフォルダに、「すべてのメール」だけにあるものはアーカイブに）、
  Google Takeout の mbox・ほかのメールソフトの mbox・.eml のアップロード（ラベル・スター・未読を引き継ぐ）。同じメールは二重に取り込まない。
  mbox への書き出し（フォルダ・スター・未読は Takeout と同じ形で書くので、取り込みで戻せる）
- **受信の通知**: 新しいメールの差出人・件名・リンクを、stui（チャット）の自分への DM・チャンネル（OAuth で連携）、
  stui・Slack・Mattermost の受信 Webhook、署名（HMAC-SHA256）付きの Webhook に送る（本文は送らない）。
  受信トレイの新着すべてか、振り分けの規則で選んだメールだけかを、通知先ごとに選べる
- **ファイル共有（skawau）との連携**: メールを書くとき skawau のファイルを選んで添付する・リンクを入れる。
  受け取った・送ったメールの添付を skawau に保存する（保存先のフォルダを選べる。既定は「メールの添付」。同じ名前は名前を変えて両方残す）。
  振り分けの規則の処理「添付を skawau に保存」で自動でも保存する
- **API・AI**: REST API、MCP（`search_messages`・`get_message`・`get_thread`・`send_message`・`update_messages`・`list_mailboxes`・`call_api` など）
- **冗長化**: アプリはステートレスで複数ノードで動く（送信待ちは SKIP LOCKED で分担、定期処理はリースで1ノードだけ）。
  PostgreSQL は Patroni、メッセージの本体は PostgreSQL か S3 互換ストレージ。HAProxy は PROXY プロトコルで本当の接続元を渡す
- **セキュリティ**: 下の「セキュリティ」を参照

設計は [docs/design.md](docs/design.md)、構築・運用は [docs/operations.md](docs/operations.md) を参照してください。

## 構成

| モジュール | 内容 |
|---|---|
| `modules/shared` | バックエンドと画面（Scala.js）で共有する API の型（circe） |
| `modules/core` | 純粋なロジック: MIME の解析（バイト位置を保つ）、IMAP の構文と応答の整形、アドレスの解析、SPF・DKIM・DMARC、迷惑メールの規則とベイズ分類、HTML の無害化、振り分けの規則、検索の構文、メッセージの組み立て |
| `modules/db` | doobie のストア、Flyway のマイグレーション、ジョブのリース、送信待ちの表の処理 |
| `modules/auth` | OIDC、APIキー・アプリパスワード、暗号化 |
| `modules/storage` | メッセージの本体の保存（PostgreSQL・S3 互換。保存時の暗号化） |
| `modules/mail` | SMTP・IMAP・POP3 のサーバー（仮想スレッド）、TLS、PROXY プロトコル、DNS、配送、迷惑メールのフィルタ、外への送信 |
| `modules/api` | http4s のルート（REST・MCP・SSE・MTA-STS・自動設定）とサービス |
| `modules/app` | 起動・設定（環境変数）・画面の配信・セキュリティのヘッダー・定期処理 |
| `modules/web` | 画面（[Scalactive](https://github.com/Ondeoma/scalactive) の `.st` 単一ファイルコンポーネント） |
| `frontend/` | Vite。Scala.js の出力と Tiptap（メールの作成のエディタ）をまとめてビルドする |
| `deploy/` | Docker（試験用の3ノード構成）と VM 構築用の設定のひな形 |

技術: Scala 3.3 LTS、Cats Effect 3、http4s 1.0（マイルストーン版）、doobie、Flyway、PostgreSQL 17 + PGroonga、Jakarta Mail（組み立て）、jsoup、Scala.js + Scalactive、Tiptap。
兄弟プロジェクト: [skiwi](https://github.com/Ondeoma/skiwi)・[skawau](https://github.com/Ondeoma/skawau)・[skea](https://github.com/Ondeoma/skea)・
[skuaka](https://github.com/Ondeoma/skuaka)・[skaka](https://github.com/Ondeoma/skaka)。

## 開発

必要なもの: JDK 21、sbt、Node.js、Docker。

```bash
docker compose -f docker-compose.dev.yml up -d --wait   # PostgreSQL+PGroonga (:54370)、Mailpit（外への送信の受け先。画面 :8032）
scripts/dev-restart.sh                                   # バックエンドを :8095 で起動（scripts/dev-env.sh の設定。DEV_LOGIN=true）
(cd frontend && npm install && npm run dev)              # 画面の開発サーバー http://localhost:5179（/api などは :8095 へ中継）
sbt ~web/fastLinkJS                                      # 画面の変更を再リンクし続ける
```

- 開発用のログイン（`DEV_LOGIN=true`）では、名前を入れるだけでログインできます。本番では使わないでください。
- 開発環境のメールのポートは 2525（SMTP）・2587（submission）・2465（SMTPS）・2143（IMAP）・2993（IMAPS）・2110（POP3）・2995（POP3S）です。
  TLS の証明書を設定しないときは自己署名の証明書を作ります。
- 外への送信はすべて Mailpit（http://localhost:8032）に届きます（`RELAY_HOST`）。
- `scripts/smoke.sh` で API を一通り確認できます。
- `(cd frontend && npm run e2e)` で画面の E2E テスト（Playwright）を動かせます。上の開発用のサーバーを起動してから実行します。ブラウザはこの機械の Google Chrome を使います（`E2E_CHANNEL` で変えられる）。

テスト（DB を使うテストは `TEST_DATABASE_URL` を設定したときだけ動きます）:

```bash
TEST_DATABASE_URL=jdbc:postgresql://localhost:54370/skereru_test sbt test
```

`modules/mail` のテストは、実際に SMTP・IMAP・POP3 を待ち受けて Jakarta Mail のクライアントで送受信します。

冗長構成の試験（Docker で PostgreSQL 3台・アプリ 3台・HAProxy）:

```bash
scripts/ha-up.sh                          # 入口 http://localhost:8196、SMTP :2526、IMAPS :2994
python3 scripts/ha-test.py --failover     # 送受信・PROXY プロトコル・ノードと DB のプライマリの停止
```

**DB マイグレーション**（`modules/db/src/main/resources/db/migration`）: スキーマを変えるときは新しい番号の `V<n>__*.sql` を追加し、適用済みのファイルは書き換えないでください。

## 配布・起動

```bash
scripts/dist.sh                   # dist/lib/*.jar と dist/static（画面）を作る
java -cp 'dist/lib/*' skereru.app.Main
```

主な環境変数（すべては `modules/app/src/main/scala/skereru/app/Main.scala` の `Config`）:

| 変数 | 内容 |
|---|---|
| `DATABASE_URL` / `DATABASE_USER` / `DATABASE_PASSWORD` | PostgreSQL（PGroonga が必要）。冗長構成では3台を並べて `targetServerType=primary` |
| `PUBLIC_URL` | Web の公開 URL（`https://` なら Cookie は `__Host-` 付き・Secure、HSTS を付ける） |
| `OIDC_ISSUER` / `OIDC_CLIENT_ID` / `OIDC_CLIENT_SECRET` | OIDC。Redirect URI は `${PUBLIC_URL}/auth/callback`。ログアウトでは IdP からもログアウトし、`${PUBLIC_URL}/` に戻る（sruru 以外の IdP では、この戻り先も登録する） |
| `OIDC_GROUPS_CLAIM` / `ADMIN_GROUPS` | 所属グループのクレーム名（既定 `groups`）と、管理者にするグループ。グループはログインのたびに IdP から同期する |
| `ADMIN_SESSION_HOURS` | 管理者のセッションの長さ（既定 8 時間。一般のユーザーは 14 日）。IdP で管理者のグループから外したら、遅くともこの時間で権限がなくなる。APIキーでのグループによる管理者の権限は、最後のログインから 7 日まで |
| `OIDC_TRUST_EMAIL` | `email_verified` を返さない IdP の email を確かめたものとして扱う（既定 false）。利用者が自分で email を変えられる IdP（マルチテナントの Entra ID など）では、ほかの人のアカウントを乗っ取れるので有効にしない |
| `AUTO_CREATE_USERS` | IdP でログインした未登録の人のユーザーを作るか（既定 true。false なら管理者が作ったユーザーだけ） |
| `APP_SECRET` | 秘密の値（DKIM の鍵・保存したメッセージ）の暗号化に使う値。全ノード共通・32文字以上を推奨。秘密の値（`APP_SECRET`・`DATABASE_PASSWORD`・`OIDC_CLIENT_SECRET` など）が `change-me` のままだと起動しない |
| `ENCRYPT_AT_REST` | メッセージを暗号化して保存する（既定 true） |
| `MAIL_HOSTNAME` | メールサーバーの名前（HELO・バナー・MX・証明書の名前） |
| `TLS_CERT` / `TLS_KEY` | TLS の証明書と秘密鍵（PEM）。更新されたら読み直す |
| `SMTP_PORT` / `SUBMISSION_PORT` / `SMTPS_PORT` / `IMAP_PORT` / `IMAPS_PORT` / `POP3_PORT` / `POP3S_PORT` | 待ち受けるポート（既定は標準のポート。`off` で待ち受けない） |
| `HTTP_PORT` / `HTTP_BIND` | Web の待ち受け（既定 `8080`・`0.0.0.0`。`DEV_LOGIN=true` のときは既定で `127.0.0.1`） |
| `MAIL_BIND` | メールの待ち受けアドレス（既定 `0.0.0.0`。`DEV_LOGIN=true` のときは既定で `127.0.0.1`） |
| `PROXY_PROTOCOL_FROM` | PROXY プロトコルを受け付ける接続元（ロードバランサー）の CIDR。ロードバランサーの IP だけ（/32）にする |
| `TRUSTED_PROXIES` | `X-Forwarded-For` を信じる接続元（ロードバランサー）の CIDR（既定はループバックだけ）。ログ・監査の IP と、回数の制限で数える IP を決める |
| `RATE_LIMIT_AUTH` / `RATE_LIMIT_PUBLIC` | IP ごとの1分あたりの回数の上限。ログインと OAuth の認可の画面（`/auth/…`・`/oauth/authorize`、既定 20）と、OAuth のトークンの発行・失効（`/oauth/token`・`/oauth/revoke`）（既定 120）。0 なら制限しない。ノードごとに数える。超えたら 429（`SKERERU-1018`）と `Retry-After` |
| `MAX_MESSAGE_MB` / `MAX_RECIPIENTS` / `MAX_RECIPIENTS_PER_HOUR` / `DEFAULT_QUOTA_MB` | メッセージの大きさ・宛先の数・1時間に外へ送れる宛先の数・容量の既定値 |
| `SPAM_JUNK_SCORE` / `SPAM_REJECT_SCORE` | 迷惑メールに振り分ける点・受信を拒否する点（既定 5・15） |
| `DNS_SERVERS` / `DNSBL_ZONES` | 問い合わせる DNS サーバー・DNSBL のゾーン（カンマ区切り） |
| `CLAMAV` / `GREYLIST` | ClamAV（`host:port`）・グレイリスト（`true`） |
| `RELAY_HOST` / `RELAY_PORT` / `RELAY_TLS` / `RELAY_USER` / `RELAY_PASSWORD` | 外への送信を中継するサーバー（未設定なら MX へ直接） |
| `OUTBOUND_VERIFY_TLS` / `OUTBOUND_REQUIRE_TLS` | 外への送信で相手の証明書を確かめる・TLS を必須にする |
| `MTA_STS_MODE` | MTA-STS のポリシー（`testing`・`enforce`・`none`） |
| `S3_ENDPOINT` / `S3_BUCKET` / `S3_ACCESS_KEY` / `S3_SECRET_KEY` | メッセージの本体を S3 互換ストレージに置く（未設定なら PostgreSQL） |
| `NODE_ID` | ノード名（ログ・定期処理のリース） |
| `OTEL_EXPORTER_OTLP_ENDPOINT` / `OTEL_EXPORTER_OTLP_HEADERS` | ログ・メトリクス・トレースの送り先（skorimako の URL と `Authorization=Bearer sko_…`（ingest のキー））。未設定なら送らない。そのほか OpenTelemetry の標準の `OTEL_*` が使える |
| `LOG_FORMAT` / `LOG_LEVEL` | 標準出力のログの形（`text`（既定）・`json`）と水準（既定 `INFO`）。ログにはトレースの ID と操作したユーザーが付く |
| `SKORIMAKO_RUM_TOKEN` / `SKORIMAKO_RUM_URL` / `SKORIMAKO_RUM_SAMPLE_RATE` | 画面のブラウザの観測（skorimako の RUM）。トークンは skorimako の管理の画面で登録したアプリのもの。URL を省略すると `SKORIMAKO_URL` |
| `VAPID_SUBJECT` | プッシュ通知（PWA）の連絡先（`mailto:…` か `https://…`。省略すると `PUBLIC_URL`）。VAPID の鍵は初回の起動で作って DB に保存する |
| `SKUAKA_URL` | 兄弟のカレンダー skuaka の URL（`https://`）。設定すると、会議の招待を skuaka に追加できる（利用者は設定の「連携」で skuaka の API トークンを入れる） |
| `STUI_URL`・`STUI_CLIENT_ID`・`STUI_CLIENT_SECRET` | 兄弟のチャット stui との連携（3つとも設定したときだけ）。利用者は設定の「連携」で stui と連携し、受信の通知を stui の DM・チャンネルに送れる。stui の `OAUTH_CLIENTS` に skereru を登録する（`redirectUri` は `{PUBLIC_URL}/api/integrations/stui/callback`、`scopes` は `["read","write"]`）。`STUI_INTERNAL_URL` はサーバーから呼ぶ URL（省けば `STUI_URL`） |
| `SKAWAU_URL`・`SKAWAU_CLIENT_ID`・`SKAWAU_CLIENT_SECRET` | 兄弟のファイル共有 skawau との連携（3つとも設定したときだけ）。利用者は設定の「連携」で skawau と連携し、skawau のファイルを添付する・リンクを入れる・添付を skawau に保存する。skawau の `OAUTH_CLIENTS` に skereru を登録する（`redirectUri` は `{PUBLIC_URL}/api/integrations/skawau/callback`、`scopes` は `["read","write"]`）。`SKAWAU_INTERNAL_URL` はサーバーから呼ぶ URL（省けば `SKAWAU_URL`） |
| `NOTIFY_ALLOW_INTERNAL` | 利用者が決めた通知先（受信 Webhook・Webhook）のうち、内部のアドレスでも送ってよい相手（カンマ区切りの CIDR・IP・ホスト名）。既定は外のアドレスだけ。`STUI_URL`・`STUI_INTERNAL_URL` のホストは自動で許す |
| `OAUTH_CLIENTS` | skereru を OAuth（認可コード + PKCE）で使う外部のサービス（skakapo など）。JSON の配列: `[{"id":"skakapo","secret":"…（16文字以上）","redirectUri":"https://…/api/connections/callback","name":"skakapo","scopes":["read","write","send"]}]`。`scopes` はそのクライアントが求めてよい範囲（read・write・send。省くと read だけ）。入口は `/oauth/authorize`・`/oauth/token`（クライアントの認証は Basic。アクセストークンは1時間で、refresh_token で更新）・`/oauth/revoke`。利用者は設定の「APIキー・MCP」の「連携しているサービス」で確認・取り消しができる |
| `DEV_LOGIN` / `ALLOW_PLAIN_AUTH` | 開発用のログイン・暗号化されていない接続での認証（開発用。本番では使わない） |

## メールソフト・API・MCP

- メールソフト: 画面の「設定 → メールソフト」でアプリパスワードを発行し、ユーザー名にメールアドレスを使います。
  Thunderbird などは `autoconfig.<ドメイン>` の自動設定に対応しています。
- API: 「設定 → APIキー・MCP」で APIキー（`skr_` で始まる）を発行し、`Authorization: Bearer <キー>` で送ります。
  スコープは `read`（参照）・`write`（整理・設定）・`send`（送信）・`admin`（管理。管理者のみ）。REST API の一覧: `GET /api/endpoints`。
- 取り込み: 「設定 → 取り込み」で、Gmail などの IMAP サーバー（Gmail は `imap.gmail.com`・993番とアプリ パスワード）か、mbox・.eml のファイルを指定します。
  IMAP の接続先が内部のアドレスを指すときはつなぎません（`OUTBOUND_ALLOW_PRIVATE=true` の試験環境を除く）。パスワードは暗号化して置き、取り込みが終わると消します。
- MCP: `POST /mcp`（Streamable HTTP）。例（Claude Code）:

```bash
claude mcp add --transport http skereru https://mail.example.com/mcp --header "Authorization: Bearer skr_xxxxxxxx"
```

## エージェント専用アカウント（skakapo などの AI エージェント）

外の AI エージェント（skakapo など）が MCP（`/mcp`）と REST API を使うための、ログインできない専用のアカウントです。
管理者が「管理 → エージェント」で作り、APIキーを発行します（ユーザー本人の APIキーと違い、人のアカウントに結び付きません）。

- アカウントは `app_user` の1行で、`oidc_sub` が `agent:<uuid>`・email なし。IdP・開発用のログインからは使えず、同じメールアドレスの IdP のユーザーに結び付けられることもありません。
- エージェントは**このアカウント自身の権限**で動きます。見られるのはこのアカウントのメールボックスだけです。ほかの宛先のメールを扱わせたいときは、
  「管理 → アドレス・別名」で配送先をこのアカウントにする・転送先に加えるなど、ふつうの方法で権限を与えます。送信には差出人に使えるアドレスと `send` のスコープが必要です。
- エージェントの送信・整理などは、このアカウントが行ったものとして記録されます（監査ログの経路は `mcp`・`api`）。
- キーのスコープは `read`・`write`・`send` から選びます（`admin` は付けられません）。アカウントを無効にすると、そのキーはすべて使えなくなります。
- 管理の API（`/api/admin/agents`）は管理者の画面からのみ使えます（APIキーでは使えません）。操作は監査ログに `agent.*` で残ります。

設定の手順:

1. 「管理 → エージェント」で名前（例: skakapo）と、必要なら送信に使うアドレス（登録済みのドメインのもの）を入れて作成します。
2. 「キーを発行」でスコープと有効期限を選んで発行し、表示されたキーをコピーします（二度と表示されません）。
3. skakapo の管理画面の「サービス」→ skereru →「エージェント専用アカウント」に貼り付けます。
4. 必要なら「アドレス・別名」で、エージェントに扱わせたいアドレスの配送先・転送先にこのアカウントを加えます。

| メソッド | パス | 内容 |
|---|---|---|
| GET | `/api/admin/agents` | 一覧（アドレス・APIキーつき。キーのハッシュは返さない） |
| POST | `/api/admin/agents` | 作る `{name, address?}`（名前は1〜60文字） |
| PATCH | `/api/admin/agents/{id}` | 名前を変える・無効にする `{name?, disabled?}` |
| POST | `/api/admin/agents/{id}/keys` | APIキーを発行 `{name, scopes, expiresDays?}`。平文のキーは応答で1回だけ返す |
| DELETE | `/api/admin/agents/{id}/keys/{keyId}` | APIキーを失効 |

## セキュリティ

- **通信**: TLS 1.2 以上・前方秘匿性のある AEAD の暗号だけ。暗号化されていない接続ではパスワードを受け付けない（IMAP は LOGINDISABLED・
  POP3/SMTP は STLS/STARTTLS の後だけ AUTH）。STARTTLS の直後に送られた平文のコマンドは捨てる（STARTTLS インジェクション対策）
- **SMTP**: 中継の拒否（オープンリレーにならない）、単独の LF・CR を含む DATA の拒否（SMTP スマグリング対策）、送信は認証が必須で
  From・MAIL FROM は自分のアドレスに限る、1時間に送れる宛先の数の上限、存在しない宛先の探索を遅らせて切る、自分たちのドメインを名乗る認証されていないメールの検出、
  外から届いた自分のホスト名の `Authentication-Results`・判定結果のヘッダーは取り除く
- **認証**: アプリパスワード・APIキー・セッションはハッシュだけを保存。IP・アカウントごとの総当たり対策（失敗が続くと15分ロック。全ノードで共有）、
  OIDC は PKCE・state・nonce を検証、ログインの戻り先は同じサイトのパスだけ
- **Web**: Cookie は HttpOnly・SameSite=Lax（https では `__Host-`・Secure）、更新系の要求は独自ヘッダーを必須にして CSRF を防ぐ、
  CSP・HSTS・X-Frame-Options・nosniff・Referrer-Policy・Permissions-Policy・COOP
- **HTML メール**: サーバーで無害化（スクリプト・イベント属性・フォーム・危険な CSS を除く）したうえで、別の URL から
  `sandbox`（スクリプト不可）と厳しい CSP 付きで iframe に表示。外部の画像は既定で読み込まない（開封の追跡を防ぐ）。添付ファイルは
  ダウンロード（`nosniff`・`sandbox`）として渡し、画像以外をブラウザで開かない
- **保存**: メッセージは APP_SECRET から作った鍵で AES-256-GCM で暗号化して保存（ID は内容の HMAC。DB やストレージが漏れても読めない）。
  DKIM の秘密鍵も暗号化して保存
- **監査**: ログイン・失敗・ロック・APIキー/アプリパスワードの発行と失効・管理の操作を監査ログに残す
- **外への接続**: 外への送信・IMAP の取り込みは、接続先が内部のアドレス（プライベート・ループバック・予約など）ならつながず、確かめた IP に固定してつなぐ
  （DNS リバインディング対策）。skuaka・stui・skawau の URL は管理者だけが決める。利用者が決めた通知先の URL も、送るたびに確かめて内部へは送らない（`NOTIFY_ALLOW_INTERNAL` で許した相手を除く）
- **MCP**: メールの本文に書かれた指示で操作されないよう、MCP からは振り分けの規則・個人の設定・管理の API を変えられない（読むだけ）
- **DoS 対策**: 接続数（IP・/64・/48 ごと）、認証前のやりとりの時間、DATA に使うメモリと時間、MIME の部品数、検索の語の数、SSE の同時接続に上限。
  計算量が入力の二乗になる正規表現を使わない
