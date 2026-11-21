# 開発用の環境変数（source scripts/dev-env.sh）。docker-compose.dev.yml の DB と Mailpit を使う
export DATABASE_URL=jdbc:postgresql://localhost:54370/skereru DATABASE_USER=skereru DATABASE_PASSWORD=skereru
export PUBLIC_URL=http://localhost:8095 HTTP_PORT=8095
export DEV_LOGIN=true
# 回数の制限（IP ごとの1分あたり）。手元ではすべての要求が 127.0.0.1 から来るので、開発中は緩める
export RATE_LIMIT_AUTH=${RATE_LIMIT_AUTH:-300}
export MAIL_HOSTNAME=localhost
# 特権ポートを避ける（本番は既定の 25・587・465・143・993・110・995）
export SMTP_PORT=2525 SUBMISSION_PORT=2587 SMTPS_PORT=2465 IMAP_PORT=2143 IMAPS_PORT=2993 POP3_PORT=2110 POP3S_PORT=2995
# 外への送信はすべて Mailpit（http://localhost:8032）に送る
export RELAY_HOST=localhost RELAY_PORT=1032 RELAY_TLS=none
# OAuth の提供側: skakapo が利用者の権限で使う（送信は send のスコープ）
export OAUTH_CLIENTS='[{"id":"skakapo","secret":"skakapo-dev-secret-0123456789","redirectUri":"http://localhost:5199/api/connections/callback","name":"skakapo","scopes":["read","write","send"]}]'
# stui（チャット）への受信の通知（stui の OAUTH_CLIENTS に skereru を登録してある. 画面は :5178、サーバーから呼ぶのは :8098）
export STUI_URL=http://localhost:5178 STUI_INTERNAL_URL=http://localhost:8098 STUI_CLIENT_ID=skereru STUI_CLIENT_SECRET=skereru-dev-secret-0123456789
# skawau（ファイル共有）: 添付を skawau から付ける・skawau に保存する（skawau の OAUTH_CLIENTS に skereru を登録してある）
export SKAWAU_URL=http://localhost:8092 SKAWAU_CLIENT_ID=skereru SKAWAU_CLIENT_SECRET=skereru-dev-secret-0123456789
