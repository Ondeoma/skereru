<script lang="scala">
import skereru.web.*
import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// アドレス帳（自分で保存した連絡先と、宛先として使ったので自動で覚えたアドレス）
case class State(tab: RV[String] = RV("saved"),
                 q: RV[String] = RV(""),
                 list: RV[List[AddressBookDto]] = RV(Nil),
                 loaded: RV[Boolean] = RV(false),
                 editing: RV[Boolean] = RV(false),
                 isNew: RV[Boolean] = RV(false),
                 address: RV[String] = RV(""),
                 name: RV[String] = RV(""),
                 company: RV[String] = RV(""),
                 phone: RV[String] = RV(""),
                 note: RV[String] = RV(""))

def initState() = State()

def mounted(s: State): Unit = { pageTitle("連絡先"); load(s) }

def load(s: State): Unit =
  Api.get[List[AddressBookDto]]("/api/address-book" + Api.query("q" -> Some(s.q.v), "all" -> Some((s.tab.v == "all").toString)))
    .foreach { l => s.list := l; s.loaded := true }

def tab(s: State, t: String): Unit = { s.tab := t; load(s) }
def tabClass(s: State, t: String) = s.tab.mapC(x => if (x == t) "active" else "")

def opt(v: String): Option[String] = Some(v.trim).filter(_.nonEmpty)

def startNew(s: State): Unit = {
  s.isNew := true; s.address := ""; s.name := ""; s.company := ""; s.phone := ""; s.note := ""; s.editing := true
}

def edit(s: State, c: AddressBookDto): Unit = {
  s.isNew := false; s.address := c.address; s.name := c.name.getOrElse(""); s.company := c.company.getOrElse("")
  s.phone := c.phone.getOrElse(""); s.note := c.note.getOrElse(""); s.editing := true
}

def save(s: State): Unit =
  Toasts.report(Api.post[UpdatedDto]("/api/address-book", SaveContactReq(s.address.v.trim, opt(s.name.v), opt(s.company.v), opt(s.phone.v), opt(s.note.v)).asJson)) { _ =>
    s.editing := false; Toasts.show("連絡先を保存しました"); load(s)
  }

def remove(s: State, c: AddressBookDto): Unit =
  Dialog.confirm(s"${c.name.getOrElse(c.address)} を連絡先から削除しますか？", "削除") {
    Toasts.report(Api.send(dom.HttpMethod.DELETE, "/api/address-book" + Api.query("address" -> Some(c.address)))) { _ => load(s) }
  }

def write(c: AddressBookDto): Unit = Store.newMail(List(MailText.quoted(c.name, c.address)))
def initial(c: AddressBookDto): String = Fmt.initials(c.name.getOrElse(c.address))
def avatar(c: AddressBookDto): String = s"background: hsl(${Fmt.hue(c.address)} 55% 45%)"
def isEmpty(s: State) = s.loaded.mapC(l => l && s.list.v.isEmpty)
def editTitle(s: State) = s.isNew.mapC(n => if (n) "連絡先を追加" else "連絡先を編集")
</script>

<template>
  <div class="page">
    <div class="row head">
      <h1 class="grow">連絡先</h1>
      <button class="primary" @click="startNew(s)">＋ 連絡先を追加</button>
    </div>
    <nav class="tabs">
      <button :class="tabClass(s, &quot;saved&quot;)" @click="tab(s, &quot;saved&quot;)">保存した連絡先</button>
      <button :class="tabClass(s, &quot;all&quot;)" title="メールを送った・受け取った相手（自動で覚えたもの）も含めて、よく使う順" @click="tab(s, &quot;all&quot;)">よく使う宛先</button>
    </nav>
    <form class="row search" @submit.prevent="load(s)">
      <input type="search" v-model="s.q" placeholder="名前・アドレス・会社で探す" class="grow" @input="load(s)">
    </form>

    <section v-if="s.editing" class="panel">
      <h2>{{ editTitle(s) }}</h2>
      <form @submit.prevent="save(s)">
        <div class="grid">
          <label>メールアドレス<input type="email" v-model="s.address" :disabled="s.isNew.mapC(!_)" required></label>
          <label>名前<input type="text" v-model="s.name" placeholder="山田 太郎"></label>
          <label>会社・所属<input type="text" v-model="s.company"></label>
          <label>電話<input type="tel" v-model="s.phone"></label>
        </div>
        <label>メモ<textarea v-model="s.note" rows="3"></textarea></label>
        <div class="row"><button type="submit" class="primary">保存</button><button type="button" @click="s.editing := false">キャンセル</button></div>
      </form>
    </section>

    <div class="cards">
      <div v-for="c in s.list" :key="c.address" class="contact">
        <span class="av" :style="avatar(c)">{{ initial(c) }}</span>
        <div class="grow info">
          <div><b>{{ c.name.getOrElse(c.address) }}</b> <span v-if="!c.saved" class="muted small" title="宛先として使ったので、自動で覚えたアドレスです">（自動）</span></div>
          <div class="small muted">{{ c.address }}</div>
          <div v-if="c.company.isDefined || c.phone.isDefined" class="small">{{ c.company.getOrElse("") }} {{ c.phone.getOrElse("") }}</div>
          <div v-if="c.note.isDefined" class="small muted note">{{ c.note.getOrElse("") }}</div>
        </div>
        <div class="acts">
          <button class="ghost" title="この人にメールを書く" @click="write(c)">✉️</button>
          <button class="ghost" :title='if (c.saved) "編集" else "連絡先として保存"' @click="edit(s, c)">✏️</button>
          <button class="ghost" title="連絡先から削除（宛先の候補にも出なくなります）" @click="remove(s, c)">🗑</button>
        </div>
      </div>
    </div>
    <p v-if="isEmpty(s)" class="muted">連絡先はまだありません。「＋ 連絡先を追加」か、「よく使う宛先」から保存できます。</p>
  </div>
</template>

<style scoped>
.head { align-items: center; }
.search { margin: .6rem 0 1rem; }
.grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: .6rem; margin-bottom: .6rem; }
label { display: flex; flex-direction: column; gap: .2rem; font-size: .85rem; }
.cards { display: flex; flex-direction: column; }
.contact { display: flex; align-items: center; gap: .8rem; padding: .6rem .4rem; border-bottom: 1px solid var(--line); }
.av { width: 36px; height: 36px; border-radius: 50%; color: #fff; display: inline-flex; align-items: center; justify-content: center; font-weight: 700; flex: none; }
.info { min-width: 0; }
.note { white-space: pre-wrap; }
.acts { display: flex; gap: .1rem; }
</style>
