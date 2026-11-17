#!/usr/bin/env bash
# 開発サーバー（scripts/dev-restart.sh）の API を一通り確かめる。DEV_LOGIN=true が必要
set -euo pipefail
B=${BASE:-http://localhost:8095}
J=$(mktemp -d)
trap 'rm -rf "$J"' EXIT
H=(-H 'Content-Type: application/json' -H 'X-Skereru: 1')
ok() { echo "OK  $*"; }
ng() { echo "NG  $*"; exit 1; }

curl -sf -c "$J/c" "${H[@]}" -d '{"name":"管理者","email":"admin@smoke.example","admin":true}' "$B/auth/dev-login" >/dev/null && ok dev-login
D=smoke$RANDOM.example
DID=$(curl -sf -b "$J/c" "${H[@]}" -d "{\"name\":\"$D\",\"autoProvision\":true}" "$B/api/admin/domains" | jq -r .id) && ok "domain $D"
curl -sf -b "$J/c" "$B/api/admin/domains/$DID/dns" | jq -e 'map(select(.kind=="TXT" and (.value|startswith("v=DKIM1"))))|length==1' >/dev/null && ok dns-records
curl -sf -b "$J/c" "${H[@]}" -d "{\"name\":\"太郎\",\"address\":\"taro@$D\",\"email\":\"taro$RANDOM@idp.example\"}" "$B/api/admin/users" >/dev/null && ok create-user
# 管理者自身にもアドレスを付ける
ME=$(curl -sf -b "$J/c" "$B/api/me" | jq -r .id)
curl -sf -b "$J/c" "${H[@]}" -d "{\"address\":\"admin@$D\",\"userId\":\"$ME\",\"forwardTo\":[],\"canSend\":true}" "$B/api/admin/addresses" >/dev/null && ok address
# CSRF: X-Skereru のない更新は拒否
code=$(curl -s -o /dev/null -w '%{http_code}' -b "$J/c" -H 'Content-Type: application/json' -d '{"name":"x"}' "$B/api/mailboxes")
[ "$code" = 403 ] && ok "csrf-guard ($code)" || ng "csrf-guard $code"
# 送信（自分と taro と外）
curl -sf -b "$J/c" "${H[@]}" -d "{\"from\":\"admin@$D\",\"to\":[\"taro@$D\",\"外の人 <someone@example.net>\"],\"cc\":[],\"bcc\":[\"admin@$D\"],\"subject\":\"スモークテスト\",\"text\":\"本文です\",\"html\":\"<p>本文<script>x</script></p>\",\"uploadIds\":[],\"inReplyTo\":null,\"forwardOf\":null,\"draftId\":null}" "$B/api/send" | jq -e '.queued==1 and .delivered==2' >/dev/null && ok send
INBOX=$(curl -sf -b "$J/c" "$B/api/mailboxes" | jq -r '.[]|select(.specialUse=="inbox")|.id')
MID=$(curl -sf -b "$J/c" "$B/api/messages?mailboxId=$INBOX" | jq -r '.items[0].id')
curl -sf -b "$J/c" "$B/api/messages/$MID" | jq -e '.summary.subject=="スモークテスト"' >/dev/null && ok message-detail
curl -sf -D "$J/h" -b "$J/c" "$B/api/messages/$MID/html" -o "$J/html"
grep -qi "content-security-policy: sandbox" "$J/h" && ! grep -q "<script" "$J/html" && ok html-sandboxed
curl -sf -b "$J/c" "$B/api/messages?q=%E3%82%B9%E3%83%A2%E3%83%BC%E3%82%AF" | jq -e '.total>=1' >/dev/null && ok search
# APIキー（read のみ）と MCP
KEY=$(curl -sf -b "$J/c" "${H[@]}" -d '{"name":"smoke","scopes":["read"]}' "$B/api/me/api-keys" | jq -r .key)
curl -sf -H "Authorization: Bearer $KEY" -H 'Content-Type: application/json' -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' "$B/mcp" \
  | jq -e '[.result.tools[].name]|(index("search_messages")!=null) and (index("send_message")==null)' >/dev/null && ok mcp-tools-by-scope
curl -sf -H "Authorization: Bearer $KEY" -H 'Content-Type: application/json' -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"search_messages","arguments":{"query":"スモーク"}}}' "$B/mcp" \
  | jq -e '.result.isError==false' >/dev/null && ok mcp-search
code=$(curl -s -o /dev/null -w '%{http_code}' -H "Authorization: Bearer $KEY" -H 'Content-Type: application/json' -d '{}' "$B/api/send")
[ "$code" = 403 ] && ok "read-key cannot send ($code)" || ng "read-key send $code"
# アプリパスワード
curl -sf -b "$J/c" "${H[@]}" -d '{"name":"smoke"}' "$B/api/me/app-passwords" | jq -e '.password|length==19' >/dev/null && ok app-password
# 振り分けの規則・スレッド・管理の画面の API
curl -sf -b "$J/c" "${H[@]}" -d '{"name":"テスト","enabled":true,"matchAll":true,"conditions":[{"field":"subject","op":"contains","value":"請求","header":null}],"actions":[{"kind":"mark_read","arg":null}]}' "$B/api/filters" >/dev/null && ok filter-rule
curl -sf -b "$J/c" "$B/api/messages/$MID/thread?markRead=true" | jq -e '.messages|length==1' >/dev/null && ok thread
for p in stats queue audit users addresses; do curl -sf -b "$J/c" "$B/api/admin/$p" >/dev/null || ng "admin/$p"; done && ok admin-apis
code=$(curl -s -o /dev/null -w '%{http_code}' -H "Authorization: Bearer $KEY" "$B/api/admin/stats")
[ "$code" = 403 ] && ok "read-key cannot admin ($code)" || ng "read-key admin $code"
echo "all ok"
