#!/usr/bin/env python3
"""開発サーバー（scripts/dev-restart.sh）にデモ用のデータを入れる。標準ライブラリだけを使う。

ドメイン demo.example と、ユーザー「山田 太郎」（yamada@demo.example・管理者）・「佐藤 花子」（sato@demo.example）を作り、
外から届いたメール（HTML・添付・ISO-2022-JP・スレッド・迷惑メール）を MX のポートで送り込む。
画面では、開発用のログインで名前「山田 太郎」・メールアドレス「yamada@demo.example」としてログインする。
"""
import email.utils
import http.cookiejar
import json
import smtplib
import sys
import time
import urllib.request
from email.mime.application import MIMEApplication
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

BASE = "http://localhost:8095"
MX = ("localhost", 2525)
DOMAIN = "demo.example"


class Web:
    def __init__(self):
        self.op = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(http.cookiejar.CookieJar()))

    def call(self, method, path, body=None, ok_conflict=False):
        data = None if body is None else json.dumps(body).encode()
        req = urllib.request.Request(BASE + path, data=data, method=method, headers={"Content-Type": "application/json", "X-Skereru": "1"})
        try:
            with self.op.open(req, timeout=30) as r:
                t = r.read().decode()
                return json.loads(t) if t else None
        except urllib.error.HTTPError as e:
            if ok_conflict and e.code == 409:
                return None
            raise


def mx(msg, sender, rcpt):
    # 代入は上書きではなく追加になるので、ないときだけ付ける（Message-ID が2本あると迷惑メールの判定で点が付く）
    if "Message-ID" not in msg:
        msg["Message-ID"] = email.utils.make_msgid(domain=sender.split("@")[1])
    if "Date" not in msg:
        msg["Date"] = email.utils.formatdate(localtime=True)
    with smtplib.SMTP(*MX, timeout=60) as s:
        s.ehlo("mail." + sender.split("@")[1])
        # 改行は CRLF にそろえて送る
        raw = msg.as_bytes().replace(b"\r\n", b"\n").replace(b"\n", b"\r\n")
        s.sendmail(sender, [rcpt], raw)


def text(sender, name, to, subject, body, **headers):
    m = MIMEText(body, "plain", "utf-8")
    m["From"] = email.utils.formataddr((name, sender))
    m["To"], m["Subject"] = to, subject
    for k, v in headers.items():
        m[k.replace("_", "-")] = v
    return m


def main():
    yamada = Web()
    yamada.call("POST", "/auth/dev-login", {"name": "山田 太郎", "email": f"yamada@{DOMAIN}", "admin": True})
    yamada.call("POST", "/api/admin/domains", {"name": DOMAIN, "autoProvision": True}, ok_conflict=True)
    # ドメインを作った後にもう一度ログインすると、メールボックスが作られる
    yamada.call("POST", "/auth/dev-login", {"name": "山田 太郎", "email": f"yamada@{DOMAIN}", "admin": True})
    sato = Web()
    sato.call("POST", "/auth/dev-login", {"name": "佐藤 花子", "email": f"sato@{DOMAIN}"})
    yamada.call("POST", "/api/admin/addresses", {"address": f"info@{DOMAIN}", "userId": None,
                                                  "forwardTo": [f"yamada@{DOMAIN}", f"sato@{DOMAIN}"], "canSend": False}, ok_conflict=True)
    me = f"yamada@{DOMAIN}"
    print("ユーザー: 山田 太郎（yamada@demo.example）・佐藤 花子（sato@demo.example）、グループ: info@demo.example")

    # 普通の業務のメール
    mx(text("suzuki@partner.invalid", "鈴木 一郎", me, "来週の打ち合わせの件",
            "山田様\n\nいつもお世話になっております。鈴木です。\n来週の打ち合わせですが、11月20日（木）14時からでいかがでしょうか。\n\nよろしくお願いいたします。\n鈴木"),
       "suzuki@partner.invalid", me)
    # スレッド（返信が続く）
    first = text("tanaka@client.invalid", "田中 美咲", me, "見積書のご確認のお願い",
                 "山田様\n\n先日お話しした件の見積書を添付いたします。\nご確認のほどよろしくお願いいたします。\n\n田中")
    first["Message-ID"] = "<estimate-1@client.invalid>"
    mx(first, "tanaka@client.invalid", me)
    reply = text("tanaka@client.invalid", "田中 美咲", me, "Re: 見積書のご確認のお願い",
                 "山田様\n\n追加で、納期について補足いたします。\n12月中旬の納品を予定しております。\n\n> 先日お話しした件の見積書を添付いたします。\n\n田中",
                 In_Reply_To="<estimate-1@client.invalid>", References="<estimate-1@client.invalid>")
    mx(reply, "tanaka@client.invalid", me)
    # 添付ファイル（PDF）
    m = MIMEMultipart()
    m["From"] = email.utils.formataddr(("経理部", "keiri@client.invalid"))
    m["To"], m["Subject"] = me, "請求書（2026年11月分）"
    m.attach(MIMEText("山田様\n\n11月分の請求書をお送りします。\nお支払い期限は12月末日です。\n\n経理部", "plain", "utf-8"))
    pdf = MIMEApplication(b"%PDF-1.4\n1 0 obj<<>>endobj\ntrailer<<>>\n%%EOF\n", "pdf")
    pdf.add_header("Content-Disposition", "attachment", filename=("utf-8", "", "請求書_2026年11月.pdf"))
    m.attach(pdf)
    mx(m, "keiri@client.invalid", me)
    # HTML のニュースレター（外部の画像つき）
    h = MIMEMultipart("alternative")
    h["From"] = email.utils.formataddr(("テックニュース", "news@technews.invalid"))
    h["To"], h["Subject"] = me, "今週のテックニュース：新しいプログラミング言語の動向"
    h["List-Id"] = "<weekly.technews.invalid>"
    h["List-Unsubscribe"] = "<https://technews.invalid/unsubscribe?id=123>"
    h.attach(MIMEText("今週のニュースです。HTML 版をご覧ください。", "plain", "utf-8"))
    h.attach(MIMEText("""<html><body style="font-family:sans-serif">
      <h2 style="color:#2563eb">今週のテックニュース</h2>
      <img src="https://technews.invalid/banner.png" width="600" alt="バナー">
      <p>注目の記事を3本お届けします。</p>
      <ul><li><a href="https://technews.invalid/a">Scala 3 の新機能まとめ</a></li>
          <li><a href="https://technews.invalid/b">メールサーバーのセキュリティ入門</a></li>
          <li><a href="https://technews.invalid/c">PostgreSQL の高可用性構成</a></li></ul>
      <table border="1" cellpadding="6" style="border-collapse:collapse"><tr><th>順位</th><th>言語</th></tr>
        <tr><td>1</td><td>Scala</td></tr><tr><td>2</td><td>Rust</td></tr></table>
      <script>alert('これは実行されません')</script>
      <p onclick="alert(1)">このメールの配信を停止するには<a href="https://technews.invalid/unsubscribe">こちら</a>。</p>
    </body></html>""", "html", "utf-8"))
    mx(h, "news@technews.invalid", me)
    # ISO-2022-JP（古いメールソフトから）
    j = MIMEText("山田さん\n\n社内報の原稿、締め切りは金曜日です。\nよろしくお願いします。\n\n総務 高橋", "plain", "iso-2022-jp")
    j["From"] = email.utils.formataddr(("総務 高橋", "takahashi@" + DOMAIN))
    j["To"], j["Subject"] = me, "社内報の原稿について"
    mx(j, "takahashi@soumu.invalid", me)
    # グループのアドレス宛て（山田と佐藤の両方に届く）
    mx(text("support@vendor.invalid", "ベンダー サポート", f"info@{DOMAIN}", "【ご案内】年末年始のサポート体制について",
            "お客様各位\n\n年末年始のサポート窓口は12月29日から1月3日までお休みいたします。"), "support@vendor.invalid", f"info@{DOMAIN}")
    # 迷惑メールらしいもの（迷惑メールのフォルダに入る）
    s = text("winner@lottery.invalid", "Prize Center <ceo@bank.invalid>", me, "CONGRATULATIONS YOU WON!!! CLAIM NOW!!!",
             "You have won $1,000,000! Click http://192.0.2.10/claim now to claim your prize. Unsubscribe here.",
             Reply_To="claims@other.invalid")
    del s["Message-ID"]
    s["Message-ID"] = "<spam1@lottery.invalid>"
    mx(s, "winner@lottery.invalid", me)
    # 佐藤さんから（このサーバーの中）
    hanako = sato.call("POST", "/api/send", {"from": f"sato@{DOMAIN}", "to": [me], "cc": [], "bcc": [], "subject": "資料の共有",
                                              "text": "山田さん\n\n会議の資料を共有します。ご確認ください。\n\n佐藤", "html": None, "uploadIds": [],
                                              "inReplyTo": None, "forwardOf": None, "draftId": None})
    time.sleep(1)
    boxes = yamada.call("GET", "/api/mailboxes")
    for b in boxes:
        print(f"  {b['name']}: {b['total']} 件（未読 {b['unread']}）")
    print("画面: http://localhost:5179/login で「山田 太郎」「yamada@demo.example」としてログインしてください")


if __name__ == "__main__":
    try:
        main()
    except Exception as e:  # noqa: BLE001
        print(f"失敗しました: {e}（scripts/dev-restart.sh で開発サーバーを起動してから実行してください）")
        sys.exit(1)
