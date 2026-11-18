<script lang="scala">
import io.github.ondeoma.scalactive.components.routes.RouterComponent
import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*
import skereru.web.parts.*
import skereru.web.views.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 画面全体の枠: ヘッダー（検索・設定・メニュー）、左のメールボックスの一覧、ルートごとの画面、作成画面
case class State(ready: RV[Boolean] = RV(false), menu: RV[Boolean] = RV(false), q: RV[String] = RV(""))

def initState() = State()

val routes: PathPatternRoutes = List(
  ("/", _ => MailView()),
  ("/mail/:box", _ => MailView()),
  ("/search", _ => MailView()),
  ("/starred", _ => MailView()),
  ("/snoozed", _ => MailView()),
  (Store.ScheduledPath, _ => ScheduledView()),
  ("/contacts", _ => ContactsView()),
  ("/m/:id", r => ThreadView(id = r.pathname.groups("id"))),
  ("/settings", _ => SettingsView()),
  ("/admin", _ => AdminView()),
  ("/login", _ => LoginView()),
)

def mounted(s: State): Unit = {
  Fallback.install(routes)
  Shortcuts.install()
  Notify.install()
  // ユーザーのメニューは、メニューとボタンの外をクリックしたとき・Esc キーで閉じる
  dom.document.addEventListener("mousedown", (e: dom.MouseEvent) => if (s.menu.v) {
    val inside = e.target match {
      case el: dom.Element => el.closest(".me") != null
      case _ => false
    }
    if (!inside) s.menu := false
  })
  dom.document.addEventListener("keydown", (e: dom.KeyboardEvent) => if (s.menu.v && e.key == "Escape") s.menu := false)
  Api.clientConfig().foreach(c => Store.config := Some(c))
  if (dom.window.location.pathname.startsWith("/login")) s.ready := true
  else Store.report(Api.me()) { me =>
    Store.me := Some(me)
    Observe.setUser(me.id.toString)
    Store.loadSettings()
    Store.loadMailboxes().foreach(_ => s.ready := true)
    Store.connect()
  }
  // 検索欄に今の検索の条件を出す
  val q = new dom.URL(dom.window.location.href).searchParams.get("q")
  if (q != null) s.q := q
}

def search(s: State): Unit = {
  val q = s.q.v.trim
  Router.go(if (q.isEmpty) "/mail/inbox" else "/search?q=" + Api.enc(q))
}

def logout(): Unit = Api.logout().onComplete(r => dom.window.location.href = r.getOrElse("/"))

/** 表示中のメールボックスの URL の名前（/mail/inbox → inbox）. */
def currentBox(href: String): String = {
  val p = new dom.URL(href).pathname
  if (p == "/") "inbox" else if (p.startsWith("/mail/")) p.drop(6) else p
}

def boxClass(b: MailboxDto) = Router.href.mapC(h => if (currentBox(h) == Store.boxKey(b)) "box active" else "box")
def specials = Store.mailboxes.mapC(_.filter(_.specialUse.isDefined))
// 受信トレイの下に「スター付き」を挟むので、受信トレイとそれ以外に分ける
def inboxBox = specials.mapC(_.filter(_.specialUse.contains(SpecialUse.Inbox)))
def otherSpecials = specials.mapC(_.filterNot(_.specialUse.contains(SpecialUse.Inbox)))
def pathClass(path: String) = Router.href.mapC(h => if (currentBox(h) == path) "box active" else "box")
def goPath(path: String): Unit = { Store.nav := false; Router.go(path) }
def folders = Store.mailboxes.mapC(_.filter(_.specialUse.isEmpty))
def depth(b: MailboxDto): String = s"padding-left: ${0.9 + b.name.count(_ == '/') * 0.9}rem"
def leaf(b: MailboxDto): String = b.name.split('/').last
def userName = Store.me.mapC(_.map(_.name).getOrElse(""))
def primary = Store.me.mapC(_.flatMap(_.addresses.find(_.isPrimary)).map(_.address).getOrElse(""))
def loggedIn = Store.me.mapC(_.isDefined)
def usage = Store.me.mapC(_.map(m => s"${Fmt.bytes(m.usedBytes)} / ${Fmt.bytes(m.quotaBytes)}").getOrElse(""))
def usageBar = Store.me.mapC(_.map(m => s"width: ${math.min(100.0, m.usedBytes * 100.0 / math.max(1L, m.quotaBytes))}%").getOrElse("width: 0"))
def picture = Store.me.mapC(_.flatMap(_.avatarUrl))
/** アカウントのボタンのヒント. */
def meTip = primary.mapC(p => s"ログイン中のアカウント: $p（メニューを開く）")
def hasPrimary = primary.mapC(_.nonEmpty)
def navClass = Store.nav.mapC(o => if (o) "side open" else "side")

def newFolder(): Unit = Dialog.prompt("新しいメールボックスの名前（階層は / で区切ります。例: 仕事/請求書）", "", "作成") { n =>
  if (n.trim.nonEmpty) Store.report(Api.post[MailboxDto]("/api/mailboxes", CreateMailboxReq(n.trim).asJson)) { _ => Store.changed() }
}

def go(b: MailboxDto): Unit = { Store.nav := false; Router.go("/mail/" + Store.boxKey(b)) }
</script>

<template>
  <header class="top">
    <button v-if="loggedIn" class="ghost icon only-sp" title="フォルダの一覧" @click="Store.nav := !Store.nav.v">☰</button>
    <RouterLink to="/mail/inbox" class="logo"><span class="brand"><img src="/logo.svg" alt="" width="30" height="30"><span class="hide-sp">skereru</span></span></RouterLink>
    <form v-if="loggedIn" class="search" @submit.prevent="search(s)">
      <span class="lens">🔍</span>
      <input type="search" v-model="s.q" placeholder="メールを検索（例: from:taro has:attachment 見積）">
    </form>
    <div class="spacer"></div>
    <!-- RouterLink には title を付けられないので、普通のリンクにする -->
    <a v-if="loggedIn" href="/settings" class="icon hide-sp" title="設定（署名・自動返信・メールソフト・APIキー）" @click.prevent="Router.go(&quot;/settings&quot;)">⚙️</a>
    <div v-if="loggedIn" class="me">
      <!-- ログイン中のメインのアドレスを、ヘッダーで常に見えるようにする（どのアカウントで操作しているか分かるように） -->
      <button class="ghost me-btn" @click="s.menu := !s.menu.v" :title="meTip">
        <span v-if="hasPrimary" class="me-addr">{{ primary }}</span><Avatar :name="userName.v" :picture="picture.v" :size="32" />
      </button>
      <div v-if="s.menu" class="menu" @click="s.menu := false">
        <div class="who"><b>{{ userName }}</b><div class="muted small">{{ primary }}</div></div>
        <!-- RouterLink の a にはこのコンポーネントのスタイルが当たらないので、ログアウトと同じ普通のリンクにする -->
        <a href="/settings" @click.prevent="Router.go(&quot;/settings&quot;)">設定・メールソフト・APIキー</a>
        <a v-if="Store.isAdmin" href="/admin" @click.prevent="Router.go(&quot;/admin&quot;)">管理</a>
        <a href="#" @click.prevent="logout()">ログアウト</a>
      </div>
    </div>
  </header>
  <div class="layout">
    <nav v-if="loggedIn" :class="navClass">
      <button class="compose" @click="Store.nav := false; Store.newMail()">✏️ 作成</button>
      <a v-for="b in inboxBox" :key="b.id" :class="boxClass(b)" href="#" @click.prevent="go(b)">
        <span class="ic">{{ SpecialUse.icon(b.specialUse.getOrElse("")) }}</span><span class="grow name">{{ Store.boxLabel(b) }}</span>
        <span v-if="b.unread > 0" class="count">{{ b.unread }}</span>
      </a>
      <a v-for="v in Store.virtualLists" :key="v.path" :class="pathClass(v.path)" :href="v.path" :title="v.tip" @click.prevent="goPath(v.path)">
        <span class="ic">{{ v.icon }}</span><span class="grow name">{{ v.title }}</span>
      </a>
      <a :class="pathClass(Store.ScheduledPath)" href="/scheduled" title="予約した送信（送る前なら取り消せます）" @click.prevent="goPath(Store.ScheduledPath)">
        <span class="ic">🕒</span><span class="grow name">予約済み</span>
      </a>
      <a :class="pathClass(&quot;/contacts&quot;)" href="/contacts" title="アドレス帳（保存した連絡先・よく使う宛先）" @click.prevent="goPath(&quot;/contacts&quot;)">
        <span class="ic">👥</span><span class="grow name">連絡先</span>
      </a>
      <a v-for="b in otherSpecials" :key="b.id" :class="boxClass(b)" href="#" @click.prevent="go(b)">
        <span class="ic">{{ SpecialUse.icon(b.specialUse.getOrElse("")) }}</span><span class="grow name">{{ Store.boxLabel(b) }}</span>
        <span v-if="b.unread > 0 &amp;&amp; !b.specialUse.contains(SpecialUse.Sent)" class="count">{{ b.unread }}</span>
      </a>
      <div class="sect"><span class="grow">フォルダ</span><button class="ghost small" title="新しいフォルダ" @click="newFolder()">＋</button></div>
      <a v-for="b in folders" :key="b.id" :class="boxClass(b)" :style="depth(b)" href="#" @click.prevent="go(b)">
        <span class="ic">📁</span><span class="grow name">{{ leaf(b) }}</span>
        <span v-if="b.unread > 0" class="count">{{ b.unread }}</span>
      </a>
      <div class="quota"><div class="bar"><div :style="usageBar"></div></div><div class="muted small">{{ usage }}</div></div>
    </nav>
    <div v-if="Store.nav" class="scrim only-sp" @click="Store.nav := false"></div>
    <main>
      <RouterComponent v-if="s.ready" :routes="routes" />
    </main>
  </div>
  <div class="composers">
    <Composer v-for="d in Store.drafts" :key="d.seq.toString" :draft="d" />
  </div>
  <DialogHost />
  <div v-if="Shortcuts.showHelp" class="kbd-help" @click="Shortcuts.showHelp := false">
    <div class="panel" @click.stop="()">
      <div class="row"><h2 class="grow">キーボードのショートカット</h2><button class="ghost" title="閉じる（Esc）" @click="Shortcuts.showHelp := false">✕</button></div>
      <table class="list"><tbody>
        <tr v-for="h in Shortcuts.help" :key="h._1"><td class="nowrap"><kbd>{{ h._1 }}</kbd></td><td>{{ h._2 }}</td></tr>
      </tbody></table>
      <p class="muted small">入力欄にいるときは使えません。</p>
    </div>
  </div>
  <div class="toasts">
    <div v-for="t in Store.toasts" :key="t.id" :class='if (t.error) "toast error" else "toast"'>
      <span class="grow">{{ t.text }}</span>
      <button v-if="t.undo.isDefined" class="undo" @click="t.undo.foreach(f => f()); Store.dismiss(t.id)">{{ t.undoLabel }}</button>
      <button title="閉じる" @click="Store.dismiss(t.id)">×</button>
    </div>
  </div>
</template>

<style scoped>
.kbd-help { position: fixed; inset: 0; background: rgba(0,0,0,.4); z-index: 900; display: flex; align-items: center; justify-content: center; padding: 1rem; }
.kbd-help .panel { max-width: 520px; width: 100%; max-height: 80vh; overflow: auto; }
kbd { font-family: ui-monospace, monospace; font-size: .8rem; border: 1px solid var(--line); border-bottom-width: 2px; border-radius: 4px; padding: .05rem .35rem; background: var(--side); }
.top { position: sticky; top: 0; z-index: 100; height: var(--header); display: flex; align-items: center; gap: .6rem; padding: 0 .9rem;
  background: var(--bg); border-bottom: 1px solid var(--line); }
.brand { font-weight: 800; font-size: 1.2rem; color: var(--accent); letter-spacing: .02em; display: inline-flex; align-items: center; gap: .45rem; white-space: nowrap; }
.brand img { display: block; border-radius: 8px; }
.search { flex: 0 1 680px; display: flex; align-items: center; background: var(--side); border-radius: 10px; padding: 0 .6rem; margin-left: 1.2rem; }
.search:focus-within { background: var(--bg); box-shadow: var(--shadow); }
.search input { border: none; background: transparent; box-shadow: none !important; width: 100%; padding: .55rem .4rem; }
.lens { opacity: .6; }
.spacer { flex: 1; }
.icon { font-size: 1.15rem; padding: .25rem .45rem; border-radius: 50%; color: var(--fg); }
.icon:hover { background: var(--hover); text-decoration: none; }
.me { position: relative; }
.me-btn { padding: 2px 2px 2px .8rem; border-radius: 999px; display: inline-flex; align-items: center; gap: .5rem; border: 1px solid var(--line); }
.me-btn:hover { background: var(--hover); }
.me-addr { font-size: .86rem; font-weight: 600; max-width: 22rem; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
@media (max-width: 760px) { .me-addr { max-width: 6.5rem; font-size: .78rem; } .me-btn { padding-left: .55rem; gap: .35rem; } }
.menu { position: absolute; right: 0; top: 2.6rem; background: var(--surface); border: 1px solid var(--line); border-radius: 10px; box-shadow: var(--shadow);
  min-width: 240px; padding: .3rem; display: flex; flex-direction: column; z-index: 200; }
.menu a { padding: .5rem .7rem; border-radius: 6px; color: var(--fg); }
.menu a:hover { background: var(--hover); text-decoration: none; }
.who { padding: .5rem .7rem .7rem; border-bottom: 1px solid var(--line); margin-bottom: .3rem; }
.layout { display: flex; min-height: calc(100vh - var(--header)); }
.side { width: 248px; flex: none; padding: .7rem .5rem; position: sticky; top: var(--header); height: calc(100vh - var(--header)); overflow-y: auto;
  display: flex; flex-direction: column; gap: 1px; }
.compose { margin: .2rem .3rem .9rem; padding: .85rem 1.3rem; border-radius: 16px; background: var(--accent-weak); border: none; color: var(--fg);
  font-weight: 600; font-size: .95rem; align-self: flex-start; box-shadow: 0 1px 3px rgba(0,0,0,.12); }
.compose:hover { box-shadow: var(--shadow); background: var(--accent-weak); }
.box { display: flex; align-items: center; gap: .6rem; padding: .38rem .9rem; border-radius: 0 18px 18px 0; color: var(--fg); font-size: .9rem; margin-right: .3rem; }
.box:hover { background: var(--hover); text-decoration: none; }
.box.active { background: var(--accent-weak); color: var(--accent); font-weight: 700; }
.box .ic { width: 1.2rem; text-align: center; }
.name { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.count { font-size: .78rem; font-weight: 700; }
.sect { display: flex; align-items: center; padding: .9rem .9rem .2rem; font-size: .78rem; color: var(--muted); }
.quota { margin-top: auto; padding: 1rem .9rem .5rem; }
.bar { height: 4px; background: var(--line); border-radius: 2px; overflow: hidden; margin-bottom: .3rem; }
.bar div { height: 100%; background: var(--accent); }
main { flex: 1; min-width: 0; }
.composers { position: fixed; right: 1rem; bottom: 0; display: flex; gap: .8rem; align-items: flex-end; z-index: 900; pointer-events: none; }
.toasts .undo { color: #93c5fd; font-weight: 700; }
.scrim { position: fixed; inset: var(--header) 0 0 0; background: rgba(0,0,0,.35); z-index: 140; }
@media (max-width: 720px) {
  .side { position: fixed; left: 0; top: var(--header); z-index: 150; background: var(--bg); transform: translateX(-100%); transition: transform .2s; box-shadow: var(--shadow); }
  .side.open { transform: none; }
  .search { margin-left: 0; flex: 1 1 auto; min-width: 0; }
  .spacer { flex: 0; }
  /* 狭い画面では、入力している間だけヘッダーの幅いっぱいに広げる（ふだんはアドレスの表示に場所を譲る） */
  .search:focus-within { position: absolute; left: .6rem; right: .6rem; top: 50%; transform: translateY(-50%); z-index: 5; }
  .composers { right: 0; left: 0; }
}
</style>
