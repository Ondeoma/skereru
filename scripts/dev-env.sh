# 開発用の環境変数（source scripts/dev-env.sh）。docker-compose.dev.yml の DB と Mailpit を使う
export DATABASE_URL=jdbc:postgresql://localhost:54370/skereru DATABASE_USER=skereru DATABASE_PASSWORD=skereru
export PUBLIC_URL=http://localhost:8095 HTTP_PORT=8095
export DEV_LOGIN=true
export MAIL_HOSTNAME=localhost
# 特権ポートを避ける（本番は既定の 25・587・465・143・993・110・995）
export SMTP_PORT=2525 SUBMISSION_PORT=2587 SMTPS_PORT=2465 IMAP_PORT=2143 IMAPS_PORT=2993 POP3_PORT=2110 POP3S_PORT=2995
# 外への送信はすべて Mailpit（http://localhost:8032）に送る
export RELAY_HOST=localhost RELAY_PORT=1032 RELAY_TLS=none
