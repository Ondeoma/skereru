# skereru の構築・運用

## 構成の例（VM 3台）

| 役割 | 台数 | 内容 |
|---|---|---|
| skereru | 3 | `deploy/vm/skereru.service`・`skereru.env`（`NODE_ID` だけ変える） |
| PostgreSQL | 3 | Patroni（`deploy/vm/patroni.yml`）+ etcd（`deploy/vm/etcd.env`）。PGroonga が必要 |
| HAProxy + keepalived | 3 | `deploy/vm/haproxy.cfg`・`keepalived.conf`。VIP に MX・メールソフト・Web を向ける |
| バックアップ | — | pgBackRest（`deploy/vm/pgbackrest.conf`） |

同じ3台に全部を載せてもよい。Docker で同じ構成を試すには `scripts/ha-up.sh`（`docker-compose.ha.yml`）。
`python3 scripts/ha-test.py --failover` で、HAProxy 経由の送受信（SMTP・submission・IMAP・POP3）、PROXY プロトコル、
総当たりのロック、アプリのノードの停止、PostgreSQL のプライマリの停止（Patroni の昇格）を確かめられる。

## 最初の設定

1. `scripts/dist.sh` で `dist/` を作り、各ノードの `/opt/skereru/current/` に置く。
2. `/etc/skereru/skereru.env` を書く。`APP_SECRET` は全ノードで同じ値（**変えると保存したメールと DKIM の鍵を読めなくなる**。安全な場所に控える）。
3. TLS の証明書を用意する（Let's Encrypt など。`MAIL_HOSTNAME` と Web の名前を含める）。全ノードに同じ証明書を置く。更新されたファイルは10分以内に読み直す。
4. IdP（OIDC）にクライアントを登録する。Redirect URI は `https://<PUBLIC_URL>/auth/callback`。管理者のグループを `ADMIN_GROUPS` に。
5. 起動して、管理者でログインし「管理 → ドメイン」でドメインを追加する。「DNS の設定」に出るレコード（MX・SPF・DKIM・DMARC・MTA-STS・TLS-RPT・autoconfig）を DNS に設定する。
6. サーバーの IP の逆引き（PTR）を `MAIL_HOSTNAME` にしてもらう（プロバイダー・クラウドの設定）。逆引きがないと相手に迷惑メール扱いされやすい。
7. 「管理 → ユーザー」でユーザーを作る（またはドメインの「自動作成」を有効にする）。
8. （任意）カレンダーの skuaka と連携するときは `SKUAKA_URL` を設定する。利用者は「設定 → 連携」で skuaka の API トークンを入れる。
9. （任意）チャットの stui に受信を知らせるときは、stui の `OAUTH_CLIENTS` に skereru を登録し（`redirectUri` は `https://<PUBLIC_URL>/api/integrations/stui/callback`、`scopes` は `["read","write"]`）、
   skereru に `STUI_URL`・`STUI_CLIENT_ID`・`STUI_CLIENT_SECRET` を設定する。利用者は「設定 → 連携」で stui と連携し、通知先（DM・チャンネル）を作る。
   受信 Webhook・Webhook の通知先は、連携しなくても使える（内部の宛先へは `NOTIFY_ALLOW_INTERNAL` で許したときだけ送る）。
10. （任意）ファイル共有の skawau と連携するときは、skawau の `OAUTH_CLIENTS` に skereru を登録し（`redirectUri` は `https://<PUBLIC_URL>/api/integrations/skawau/callback`、`scopes` は `["read","write"]`）、
   skereru に `SKAWAU_URL`・`SKAWAU_CLIENT_ID`・`SKAWAU_CLIENT_SECRET` を設定する。利用者は「設定 → 連携」で skawau と連携する。

### 本番の前に確かめること（セキュリティ）

- `deploy/vm/` のテンプレートの `change-me` をすべて秘密の値に変える（`skereru.env`・`patroni.yml`・`pgbackrest.conf`・`keepalived.conf`）。アプリは `change-me` のままの秘密では起動しない。
- 内部の証明書（自前の CA）を用意し、etcd（ピア・クライアントとも TLS とクライアント証明書・ユーザー認証）、Patroni の REST API（認証と TLS）、PostgreSQL（`hostssl`・`verify-full`）を暗号化する。
- FW で次のように絞る: アプリのポート（8080・2525 など）は HAProxy のノードからだけ。Patroni の 8008・PostgreSQL の 5432・etcd の 2379/2380 はノード同士と HAProxy だけ。VRRP（プロトコル112）はノード同士だけ。
- `PROXY_PROTOCOL_FROM` と `TRUSTED_PROXIES` は HAProxy のノードの IP（/32）だけにする（サブネット全体にすると、ほかの機械から接続元を偽れる）。
- `DEV_LOGIN` は付けない。`OIDC_TRUST_EMAIL` は、利用者が email を変えられない IdP のときだけ使う。
- バックアップは `repo1-cipher-type` で暗号化し、`repo1-cipher-pass` を `APP_SECRET` と同じく安全な場所に控える。

### 外への送信（ポート25）

多くのクラウドは外向きのポート25を塞いでいます。解除を申請するか、`RELAY_HOST` で中継サーバー（SendGrid・SES・プロバイダーの SMTP など）を使ってください。
中継サーバーを使うときは、その SPF の `include:` を SPF のレコードに足します。

### MTA-STS

`mta-sts.<ドメイン>` を Web（HAProxy）に向け、その名前を含む証明書を用意すると `/.well-known/mta-sts.txt` を返します。
`MTA_STS_MODE=testing` で始め、TLS-RPT の報告で問題がないことを確かめてから `enforce` にします（`_mta-sts` の TXT の id も変える）。

## 冗長化と障害

- アプリのノードが落ちても、HAProxy が `/healthz` で外し、残りのノードが受ける。IMAP の接続は切れるが、メールソフトはつなぎ直す。
- PostgreSQL のプライマリが落ちると Patroni が昇格させる。アプリは複数ホストの JDBC URL（`targetServerType=primary`）でつなぎ直す。
  切り替えの間に受信した SMTP は 451（一時エラー）を返すので、相手が再送する。
- `/healthz` は DB のプライマリに書けないとき 503 を返すので、その間はノードに振り分けない。
- 送信待ちは DB にあるので、どのノードが落ちても残りのノードが送る。

## 日々の運用

- 「管理 → 概要」: 受信・迷惑メール・拒否・送信の数（24時間）、送信待ち・失敗の数。
- 「管理 → 送信待ち」: 送れていない宛先とエラー。再送・取り消し。
- 「管理 → 監査ログ」: ログイン・失敗・ロック・鍵の発行・管理の操作。
- 「管理 → DMARC」: ほかのメールサーバーから届いた DMARC の集計レポート。自分のドメインを名乗るメールの送信元と、認証に通ったかが分かる。
  通っていない送信元が多いときは、なりすましか、SPF・DKIM の設定漏れ（自社の別のサービスなど）。
- DKIM の鍵の作り直し: 「管理 → ドメイン → 鍵を作り直す」。新しいセレクターの TXT を追加し、古い TXT は数日後に消す。
- 迷惑メール・ゴミ箱は `JUNK_RETENTION_DAYS`・`TRASH_RETENTION_DAYS`（既定30日）で自動で消える。

## バックアップ

- PostgreSQL を pgBackRest でバックアップする（メッセージの本体を DB に置く既定の構成では、これですべて）。
- S3 互換ストレージに本体を置く場合は、ストレージ側もバックアップする（バージョニング・複製）。
- `APP_SECRET` を失うとバックアップからも読めないので、別に保管する。

## 迷惑メールの調整

- `SPAM_JUNK_SCORE`・`SPAM_REJECT_SCORE` で振り分け・拒否の点を変える。判定の内訳は、メッセージの「詳細」と `X-Spam-Status` ヘッダーで見られる。
- DNSBL（`DNSBL_ZONES`）は利用条件を確認してから使う（多くは商用・大量の利用が有料。公開の DNS リゾルバー経由では答えないものがある）。
  `DNS_SERVERS` に自前のリゾルバー（unbound など）を指定するとよい。
- ClamAV（`CLAMAV=host:port`）を使うと、ウイルスを含むメールを受信の時点で拒否する。
