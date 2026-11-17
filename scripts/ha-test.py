#!/usr/bin/env python3
"""3ノード試験環境（scripts/ha-up.sh）の確認。標準ライブラリだけを使う。

  python3 scripts/ha-test.py            # HAProxy 経由で、Web・SMTP（MX・submission）・IMAP・POP3 を一通り確かめる
  python3 scripts/ha-test.py --failover # 上に加えて、アプリのノードと PostgreSQL のプライマリを止めても使えるか確かめる
"""
import email
import http.cookiejar
import imaplib
import json
import poplib
import random
import smtplib
import ssl
import subprocess
import sys
import time
import urllib.request
from email.header import decode_header, make_header
from email.mime.text import MIMEText

BASE = "http://localhost:8196"
SMTP, SUBMISSION, IMAPS, POP3S = 2526, 2588, 2994, 2996
COMPOSE = ["docker", "compose", "-f", "docker-compose.ha.yml"]
TLS = ssl.create_default_context()
TLS.check_hostname = False
TLS.verify_mode = ssl.CERT_NONE  # 試験環境は各ノードの自己署名の証明書


def ok(msg): print(f"OK  {msg}", flush=True)
def ng(msg): print(f"NG  {msg}", flush=True); sys.exit(1)


class Web:
    def __init__(self):
        self.op = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(http.cookiejar.CookieJar()))

    def call(self, method, path, body=None):
        data = None if body is None else json.dumps(body).encode()
        req = urllib.request.Request(BASE + path, data=data, method=method,
                                     headers={"Content-Type": "application/json", "X-Skereru": "1"})
        with self.op.open(req, timeout=30) as r:
            t = r.read().decode()
            return json.loads(t) if t else None

    def login(self, name, mail=None, admin=False):
        self.call("POST", "/auth/dev-login", {"name": name, "email": mail or "", "admin": admin})
        return self


def wait_health(timeout=120):
    end = time.time() + timeout
    while time.time() < end:
        try:
            with urllib.request.urlopen(BASE + "/healthz", timeout=3) as r:
                if r.status == 200:
                    return True
        except Exception:
            pass
        time.sleep(2)
    return False


def retry(f, what, tries=30):
    """障害の直後は一時的に失敗することがあるので、何度か試す。"""
    last = None
    for _ in range(tries):
        try:
            return f()
        except Exception as e:  # noqa: BLE001
            last = e
            time.sleep(2)
    ng(f"{what}: {last}")


def clear_failures():
    """前回の試験の認証の失敗（総当たり対策の記録）を消す（プライマリで実行する）。"""
    for pg in ["pg1", "pg2", "pg3"]:
        r = subprocess.run(COMPOSE + ["exec", "-T", pg, "psql", "-h", "/tmp", "-U", "postgres", "-d", "skereru", "-tAc",
                                      "select pg_is_in_recovery()"], capture_output=True, text=True)
        if r.stdout.strip() == "f":
            subprocess.run(COMPOSE + ["exec", "-T", pg, "psql", "-h", "/tmp", "-U", "postgres", "-d", "skereru", "-c",
                                      "delete from auth_failure"], capture_output=True, check=True)
            return


def subjects_imap(user, pw):
    m = imaplib.IMAP4_SSL("localhost", IMAPS, ssl_context=TLS)
    try:
        m.login(user, pw)
        m.select("INBOX")
        _, data = m.uid("FETCH", "1:*", "(BODY.PEEK[HEADER.FIELDS (SUBJECT RECEIVED)])")
        out = []
        for part in data:
            if isinstance(part, tuple):
                msg = email.message_from_bytes(part[1])
                out.append((str(make_header(decode_header(msg.get("Subject", "")))), msg.get_all("Received") or []))
        return out
    finally:
        m.logout()


def send_mx(frm, to, subject):
    msg = MIMEText("HA の試験です", "plain", "utf-8")
    msg["Subject"], msg["From"], msg["To"] = subject, frm, to
    msg["Message-ID"] = f"<{random.random()}@sender.invalid>"
    msg["Date"] = email.utils.formatdate(localtime=True)
    with smtplib.SMTP("localhost", SMTP, timeout=30) as s:
        s.ehlo("client.sender.invalid")
        s.sendmail(frm, [to], msg.as_bytes())


def send_submission(user, pw, to, subject):
    msg = MIMEText("submission の試験です", "plain", "utf-8")
    msg["Subject"], msg["From"], msg["To"] = subject, user, to
    with smtplib.SMTP("localhost", SUBMISSION, timeout=30) as s:
        s.starttls(context=TLS)
        s.login(user, pw)
        s.sendmail(user, [to], msg.as_bytes())


def main():
    failover = "--failover" in sys.argv
    if not wait_health():
        ng("HAProxy の /healthz が応答しません（scripts/ha-up.sh を先に）")
    ok("healthz")
    clear_failures()

    # どのノードに当たっても同じデータが見えること（セッションは DB にある）
    admin = Web().login("HA 管理者", admin=True)
    dom = f"ha{random.randint(1000, 9999)}.test"
    admin.call("POST", "/api/admin/domains", {"name": dom, "autoProvision": True})
    ok(f"domain {dom}")
    users = {}
    for name in ["alice", "bob"]:
        w = Web().login(f"{name}-{dom}", f"{name}@{dom}")
        pw = w.call("POST", "/api/me/app-passwords", {"name": "ha"})["password"]
        users[name] = (f"{name}@{dom}", pw, w)
    # 起動の直後は全ノードが揃うまで少し待つ
    nodes = set()
    end = time.time() + 120
    while len(nodes) < 3 and time.time() < end:
        with urllib.request.urlopen(BASE + "/healthz") as r:
            nodes.add(r.read().decode().split()[-1])
        time.sleep(0.5)
    if len(nodes) < 3:
        ng(f"振り分け先が3台になっていません: {nodes}")
    ok(f"round-robin {sorted(nodes)}")

    alice, bob = users["alice"], users["bob"]
    # MX（25番相当）を何度か送り、どのノードに当たっても届くこと
    for i in range(6):
        send_mx("sender@sender.invalid", bob[0], f"MX {i}")
    send_submission(alice[0], alice[1], bob[0], "submission から")
    got = retry(lambda: (lambda s: s if len([x for x in s if x[0].startswith("MX ") or x[0] == "submission から"]) >= 7 else (_ for _ in ()).throw(Exception(f"{len(s)} 件")))(subjects_imap(bob[0], bob[1])),
                "IMAP で受信の確認")
    ok(f"smtp(mx)+submission → imap: {len(got)} 件")
    # PROXY プロトコル: Received に HAProxy のコンテナではなく、本当の接続元が残ること
    rec = [r for s, rs in got if s.startswith("MX ") for r in rs]
    hap = subprocess.run(COMPOSE + ["exec", "-T", "haproxy", "hostname", "-i"], capture_output=True, text=True).stdout.strip()
    if hap and any(hap in r for r in rec):
        ng(f"Received に HAProxy の IP（{hap}）が入っています: {rec[0]}")
    ok("proxy-protocol（Received: " + (rec[0].split("[")[1].split("]")[0] if rec and "[" in rec[0] else "?") + ")")
    # POP3
    p = poplib.POP3_SSL("localhost", POP3S, context=TLS)
    p.user(bob[0]); p.pass_(bob[1])
    n, _ = p.stat()
    p.quit()
    if n < 7:
        ng(f"POP3 の件数 {n}")
    ok(f"pop3 {n} 件")
    if failover:
        run_failover(admin, bob)
    brute_force(alice)
    print("all ok")


def run_failover(admin, bob):

    # アプリのノードを1台止めても使える
    subprocess.run(COMPOSE + ["stop", "app1"], check=True, capture_output=True)
    time.sleep(6)
    for i in range(4):
        retry(lambda i=i: send_mx("sender@sender.invalid", bob[0], f"app1 停止中 {i}"), "app1 停止中の送信")
    retry(lambda: admin.call("GET", "/api/admin/stats"), "app1 停止中の Web")
    ok("app1 停止中も受信・Web が使える")
    subprocess.run(COMPOSE + ["start", "app1"], check=True, capture_output=True)

    # PostgreSQL のプライマリを止める → Patroni が昇格させ、アプリはつなぎ直す
    out = subprocess.run(COMPOSE + ["exec", "-T", "pg2", "patronictl", "-c", "/etc/patroni/patroni.yml", "list", "-f", "json"],
                         capture_output=True, text=True).stdout
    leader = next(m["Member"] for m in json.loads(out) if m["Role"] == "Leader")
    t0 = time.time()
    subprocess.run(COMPOSE + ["stop", leader], check=True, capture_output=True)
    if not wait_health(180):
        ng("プライマリの停止後に healthz が戻りません")
    ok(f"{leader} を停止 → {time.time() - t0:.0f} 秒で復旧")
    retry(lambda: send_mx("sender@sender.invalid", bob[0], "フェイルオーバー後"), "フェイルオーバー後の送信")
    retry(lambda: (lambda s: s if any(x[0] == "フェイルオーバー後" for x in s) else (_ for _ in ()).throw(Exception("未着")))(subjects_imap(bob[0], bob[1])),
          "フェイルオーバー後の受信")
    ok("フェイルオーバー後も送受信できる")
    subprocess.run(COMPOSE + ["start", leader], check=True, capture_output=True)


def brute_force(alice):
    # 総当たりのロック（最後に行う。同じ IP からの試験はしばらくロックされる）（全ノードで共有）
    fails = 0
    for _ in range(11):
        try:
            m = imaplib.IMAP4_SSL("localhost", IMAPS, ssl_context=TLS)
            m.login(alice[0], "wrong password")
        except imaplib.IMAP4.error:
            fails += 1
    try:
        m = imaplib.IMAP4_SSL("localhost", IMAPS, ssl_context=TLS)
        m.login(alice[0], alice[1])
        ng("ロック中なのに正しいパスワードでログインできました")
    except imaplib.IMAP4.error:
        ok(f"brute-force lock（{fails} 回失敗の後はロック）")



if __name__ == "__main__":
    main()
