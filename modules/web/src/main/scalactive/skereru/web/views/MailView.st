<script lang="scala">
import skereru.web.*
import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// メッセージの一覧（メールボックス・検索の結果）。選んでまとめて整理できる
case class State(list: RV[List[MessageSummaryDto]] = RV(Nil),
                 total: RV[Int] = RV(0),
                 offset: RV[Int] = RV(0),
                 selected: RV[List[String]] = RV(Nil),
                 loading: RV[Boolean] = RV(true),
                 key: RV[String] = RV(""),
                 cursor: RV[Int] = RV(-1),
                 unregister: RV[Option[() => Unit]] = RV(None))

def initState() = State()

/** URL から、表示するメールボックス（なければ全体の検索）と検索の条件を読む. */
def route(): (Option[MailboxDto], String, Boolean) = {
  val p = new dom.URL(dom.window.location.href).pathname
  val q = Router.query.v.getOrElse("q", "")
  if (p.startsWith("/search")) (None, q, true)
  else if (Store.virtualList(p).isDefined) (None, Store.virtualList(p).get.query, true)
  else (Store.boxByKey(if (p.startsWith("/mail/")) p.drop(6) else "inbox"), q, false)
}

def load(s: State): Unit = {
  val (box, q, isSearch) = route()
  val key = box.map(_.id.toString).getOrElse("") + "|" + q
  if (key != s.key.v) { s.offset := 0; s.selected := Nil; s.key := key }
  if (box.isEmpty && !isSearch) { s.loading := false; s.list := Nil; s.total := 0 }
  else {
    pageTitle(box.map(Store.boxLabel).getOrElse(listTitle(q)))
    Api.messages(box.map(_.id), q, s.offset.v, Store.settings.v.pageSize).onComplete { r =>
      s.loading := false
      r.fold(Toasts.error, l => { s.list := l.items; s.total := l.total; s.selected := s.selected.v.filter(id => l.items.exists(_.id.toString == id)) })
    }
  }
}

def mounted(s: State): Unit = {
  // キーボードのショートカット（j/k で動かすカーソルの行、なければ選んだもの）
  def cur: Option[MessageSummaryDto] = s.list.v.lift(s.cursor.v)
  def targets: List[UUID] = if (s.selected.v.nonEmpty) ids(s) else cur.map(_.id).toList
  def move(d: Int): Unit = if (s.list.v.nonEmpty) {
    s.cursor := math.max(0, math.min(s.list.v.size - 1, s.cursor.v + d))
    Option(dom.document.querySelectorAll(".list .row").item(s.cursor.v)).foreach(_.asInstanceOf[dom.HTMLElement].scrollIntoView(false))
  }
  def onTargets(f: => Unit): Unit = if (targets.nonEmpty) { if (s.selected.v.isEmpty) s.selected := targets.map(_.toString); f }
  s.unregister := Some(Shortcuts.register(
    "j" -> (() => move(1)), "k" -> (() => move(-1)),
    "o" -> (() => cur.foreach(open)), "Enter" -> (() => cur.foreach(open)),
    "x" -> (() => cur.foreach(m => toggle(s, m.id.toString, !isSel(s, m.id.toString).v))),
    "s" -> (() => cur.foreach(m => star(s, m))),
    "e" -> (() => onTargets(archive(s))),
    "#" -> (() => onTargets(remove(s))),
    "b" -> (() => onTargets(snooze(s)))
  ))
  load(s)
  watch(Store.tick, false)((_, _) => load(s))
  watch(Router.href, false)((_, _) => load(s))
}

def box = computed { Router.href.v; Store.mailboxes.v; route()._1 }
def use(): Option[String] = route()._1.flatMap(_.specialUse)
def isSentLike = computed { Router.href.v; use().exists(u => u == SpecialUse.Sent || u == SpecialUse.Drafts) }
def inTrash = computed { Router.href.v; use().exists(u => u == SpecialUse.Trash || u == SpecialUse.Junk) }
def inJunk = computed { Router.href.v; use().contains(SpecialUse.Junk) }
def title = computed { Router.href.v; Store.mailboxes.v; val (b, q, _) = route(); b.map(Store.boxLabel).getOrElse(listTitle(q)) }

/** 検索の一覧の見出し（スター付きは検索ではなく、ひとつの一覧として見せる）. */
def listTitle(q: String): String = Store.virtualList(new dom.URL(dom.window.location.href).pathname).map(_.title).getOrElse(s"「$q」の検索結果")
def inSnoozed = computed { Router.href.v; new dom.URL(dom.window.location.href).pathname == "/snoozed" }

def ids(s: State): List[UUID] = s.selected.v.map(UUID.fromString)
def anySelected(s: State) = s.selected.mapC(_.nonEmpty)
def allSelected(s: State) = computed { s.list.v.nonEmpty && s.selected.v.size == s.list.v.size }
def toggleAll(s: State): Unit = s.selected := (if (allSelected(s).v) Nil else s.list.v.map(_.id.toString))

def act(s: State, r: UpdateMessagesReq, msg: String, undo: Option[UpdateMessagesReq] = None): Unit =
  Toasts.report(Api.update(r)) { n =>
    s.selected := Nil
    Toasts.show(s"${n.count} 件を$msg", action = undo.map(u => "元に戻す" -> (() => Toasts.report(Api.update(u))(_ => Store.changed()))), ms = undo.map(_ => 8000))
    Store.changed()
  }


def markRead(s: State, read: Boolean): Unit = act(s, UpdateMessagesReq.of(ids(s)).copy(seen = Some(read)), if (read) "既読にしました" else "未読にしました")
def snooze(s: State): Unit = Store.snooze(ids(s))(() => s.selected := Nil)
def unsnooze(s: State): Unit = { Store.unsnooze(ids(s)); s.selected := Nil }

def archive(s: State): Unit = Store.special(SpecialUse.Archive).foreach { a =>
  val from = box.v.map(_.id)
  act(s, UpdateMessagesReq.of(ids(s)).copy(moveTo = Some(a.id)), "アーカイブしました", from.map(f => UpdateMessagesReq.of(ids(s)).copy(moveTo = Some(f))))
}
/** 削除のボタンのヒント（ゴミ箱では完全に削除になる）. */
def deleteTip = inTrash.mapC(t => if (t) "完全に削除（元に戻せません）" else "削除（ゴミ箱へ移します）")

def remove(s: State): Unit =
  if (inTrash.v) Dialog.confirm(s"${s.selected.v.size} 件を完全に削除しますか？（元に戻せません）", "完全に削除") { act(s, UpdateMessagesReq.of(ids(s)).copy(delete = Some(true)), "完全に削除しました") }
  else {
    val from = box.v.map(_.id)
    act(s, UpdateMessagesReq.of(ids(s)).copy(delete = Some(true)), "ゴミ箱へ移動しました", from.map(f => UpdateMessagesReq.of(ids(s)).copy(moveTo = Some(f))))
  }
def spam(s: State, yes: Boolean): Unit = act(s, UpdateMessagesReq.of(ids(s)).copy(spam = Some(yes)), if (yes) "迷惑メールとして報告しました" else "迷惑メールではないと報告しました")
def move(s: State): Unit = {
  val choices = Store.mailboxes.v.filterNot(b => box.v.exists(_.id == b.id)).map(b => b.id.toString -> Store.boxLabel(b))
  Dialog.choose("移動先のメールボックス", choices, "移動") { id => act(s, UpdateMessagesReq.of(ids(s)).copy(moveTo = Some(UUID.fromString(id))), "移動しました") }
}
def empty(s: State): Unit = box.v.foreach { b =>
  Dialog.confirm(s"${Store.boxLabel(b)}のメッセージをすべて完全に削除しますか？（元に戻せません）", "空にする") {
    Toasts.report(Api.post[UpdatedDto](s"/api/mailboxes/${b.id}/empty")) { n => Toasts.show(s"${n.count} 件を削除しました"); Store.changed() }
  }
}
def renameBox(): Unit = box.v.filter(_.specialUse.isEmpty).foreach { b =>
  Dialog.prompt("新しい名前", b.name, "変更") { n =>
    Toasts.report(Api.patchUnit(s"/api/mailboxes/${b.id}", RenameMailboxReq(n.trim).asJson)) { _ => Store.changed() }
  }
}
def deleteBox(): Unit = box.v.filter(_.specialUse.isEmpty).foreach { b =>
  Dialog.confirm(s"メールボックス「${b.name}」と中のメッセージを削除しますか？", "削除") {
    Toasts.report(Api.delete(s"/api/mailboxes/${b.id}")) { _ => Store.changed(); Router.go("/mail/inbox") }
  }
}

def star(s: State, m: MessageSummaryDto): Unit =
  Toasts.report(Api.update(UpdateMessagesReq.of(List(m.id)).copy(flagged = Some(!m.flagged)))) { _ =>
    s.list := s.list.v.map(x => if (x.id == m.id) x.copy(flagged = !m.flagged) else x)
  }

def open(m: MessageSummaryDto): Unit =
  if (m.draft) Toasts.report(Api.get[ComposeReq](s"/api/drafts/${m.id}"))(r => Store.compose(r))
  else Router.go(s"/m/${m.id}")

def toggle(s: State, id: String, on: Boolean): Unit = s.selected := (if (on) (s.selected.v :+ id).distinct else s.selected.v.filterNot(_ == id))
def isSel(s: State, id: String) = s.selected.mapC(_.contains(id))

def who(m: MessageSummaryDto): String =
  if (isSentLike.v) "To: " + m.to.map(Fmt.addr).mkString(", ") else m.from.map(Fmt.addr).getOrElse("(差出人なし)")
def rowClass(m: MessageSummaryDto): String = "row" + (if (m.seen) "" else " unread")
/** キーボードのカーソルの行も示す. */
def rowClassAt(s: State, m: MessageSummaryDto) = s.cursor.mapC(c => rowClass(m) + (if (s.list.v.lift(c).exists(_.id == m.id)) " cursor" else ""))

def unmounted(s: State): Unit = s.unregister.v.foreach(f => f())
def range(s: State) = computed {
  val t = s.total.v
  if (t == 0) "" else s"${s.offset.v + 1}–${math.min(t, s.offset.v + s.list.v.size)} / $t"
}
def page(s: State, dir: Int): Unit = {
  val n = s.offset.v + dir * Store.settings.v.pageSize
  if (n >= 0 && n < s.total.v) { s.offset := n; load(s) }
}
def hasPrev(s: State) = s.offset.mapC(_ > 0)
def hasNext(s: State) = computed { s.offset.v + s.list.v.size < s.total.v }
def isEmpty(s: State) = computed { !s.loading.v && s.list.v.isEmpty }
def isFolder = computed { box.v.exists(_.specialUse.isEmpty) }
</script>

<template>
  <div class="mail">
    <div class="bar">
      <input type="checkbox" :checked="allSelected(s)" @change="toggleAll(s)" title="すべて選ぶ">
      <template v-if="anySelected(s)">
        <button v-if="inTrash.mapC(!_)" class="ghost" title="アーカイブ（受信トレイから外して保管します。検索や「アーカイブ」からいつでも見られます）" @click="archive(s)">🗄</button>
        <button v-if="inSnoozed.mapC(!_)" class="ghost" title="スヌーズ（指定した日時まで一覧から隠し、その日時に受信トレイへ戻す）" @click="snooze(s)">⏰</button>
        <button v-if="inSnoozed" class="ghost" title="スヌーズを解除して、元の場所に戻す" @click="unsnooze(s)">⏰ 解除</button>
        <button v-if="inJunk.mapC(!_)" class="ghost" title="迷惑メールとして報告（迷惑メールに移し、今後の判定に学ばせます）" @click="spam(s, true)">🚫</button>
        <button v-if="inJunk" class="ghost" @click="spam(s, false)">迷惑メールではない</button>
        <button class="ghost" :title="deleteTip" @click="remove(s)">🗑</button>
        <span class="sep"></span>
        <button class="ghost" title="選んだメールを既読にする" @click="markRead(s, true)">✉️ 既読</button>
        <button class="ghost" title="選んだメールを未読にする" @click="markRead(s, false)">未読</button>
        <button class="ghost" title="選んだメールをフォルダへ移動" @click="move(s)">📁 移動</button>
      </template>
      <template v-else>
        <!-- 「⟳」の文字はフォントによって小さく出るので、SVG のアイコンにする -->
        <button class="ghost tool-ic" title="最新の状態に更新" aria-label="更新" @click="Store.changed()"><svg viewBox="0 0 24 24" width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M20 11a8 8 0 1 0-2.3 5.7"/><path d="M20 4v7h-7"/></svg></button>
        <button v-if="inTrash" class="ghost small" @click="empty(s)">すべて完全に削除</button>
        <button v-if="isFolder" class="ghost small" @click="renameBox()">名前を変える</button>
        <button v-if="isFolder" class="ghost small" @click="deleteBox()">削除</button>
      </template>
      <span class="grow"></span>
      <span class="muted small">{{ range(s) }}</span>
      <button class="ghost tool-ic" title="前へ" aria-label="前へ" :disabled="hasPrev(s).mapC(!_)" @click="page(s, -1)"><svg viewBox="0 0 24 24" width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M15 18l-6-6 6-6"/></svg></button>
      <button class="ghost tool-ic" title="次へ" aria-label="次へ" :disabled="hasNext(s).mapC(!_)" @click="page(s, 1)"><svg viewBox="0 0 24 24" width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M9 18l6-6-6-6"/></svg></button>
    </div>
    <h1 class="title only-sp">{{ title }}</h1>
    <div class="list">
      <div v-for="m in s.list" :key="m.id" :class="rowClassAt(s, m)" @click="open(m)">
        <span class="cell" @click.stop="()"><input type="checkbox" title="選ぶ（まとめて操作する）" :checked="isSel(s, m.id.toString)" @change="toggle(s, m.id.toString, Dom.checked(event))"></span>
        <button :class='if (m.flagged) "ghost star on" else "ghost star"' :title='if (m.flagged) "スターを外す" else "スターを付ける（あとで見返す印）"' @click.stop="star(s, m)">{{ if (m.flagged) "★" else "☆" }}</button>
        <span class="from">{{ who(m) }}<span v-if="m.threadCount > 1" class="n">{{ m.threadCount }}</span></span>
        <span class="subj">
          <span v-if="m.draft" class="draft">下書き</span>
          <b>{{ if (m.subject.isEmpty) "(件名なし)" else m.subject }}</b><span class="muted"> — {{ m.preview }}</span>
        </span>
        <span class="clip">{{ if (m.hasAttachments) "📎" else "" }}</span>
        <span class="date">{{ Fmt.short(m.date) }}</span>
      </div>
      <p v-if="isEmpty(s)" class="empty">メッセージはありません</p>
      <p v-if="s.loading" class="empty">読み込み中…</p>
    </div>
  </div>
</template>

<style scoped>
.row.cursor { box-shadow: inset 3px 0 0 var(--accent); background: var(--hover); }
.mail { display: flex; flex-direction: column; }
.bar { position: sticky; top: var(--header); z-index: 50; display: flex; align-items: center; gap: .2rem; padding: .35rem .8rem; background: var(--bg);
  border-bottom: 1px solid var(--line); min-height: 46px; }
.tool-ic { width: 36px; height: 36px; padding: 0; display: inline-flex; align-items: center; justify-content: center; border-radius: 50%; }
.tool-ic svg { display: block; }
.bar input[type="checkbox"] { margin: 0 .6rem 0 .45rem; }
.sep { width: 1px; height: 20px; background: var(--line); margin: 0 .3rem; }
.title { font-size: 1.1rem; margin: .6rem .9rem; }
.row { display: grid; grid-template-columns: 34px 30px minmax(120px, 210px) 1fr 24px 76px; align-items: center; gap: .3rem; padding: 0 .9rem 0 .5rem;
  height: 42px; border-bottom: 1px solid var(--line); cursor: pointer; font-size: .9rem; background: var(--row, var(--bg)); }
.row:hover { box-shadow: inset 1px 0 0 var(--line), inset -1px 0 0 var(--line), 0 1px 2px rgba(60,64,67,.25); position: relative; z-index: 1; }
.row.unread { --row: var(--bg); font-weight: 700; }
.row:not(.unread) { --row: var(--side); }
.row:not(.unread) b { font-weight: 500; }
.cell { display: flex; justify-content: center; }
.star { padding: 0 .2rem; font-size: 1.05rem; color: var(--muted); }
.star.on { color: #f4b400; }
.from { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.n { color: var(--muted); font-weight: 400; font-size: .78rem; margin-left: .3rem; }
.subj { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-weight: 400; }
.unread .subj b { font-weight: 700; }
.draft { color: var(--danger); margin-right: .4rem; font-weight: 600; }
.date { text-align: right; font-size: .8rem; white-space: nowrap; }
.clip { text-align: center; }
@media (max-width: 720px) {
  .row { grid-template-columns: 28px 24px 1fr 60px; grid-template-rows: auto auto; height: auto; padding: .45rem .6rem .45rem .2rem; row-gap: .1rem; }
  .from { grid-column: 3; }
  .date { grid-column: 4; }
  .subj { grid-column: 3 / span 2; grid-row: 2; }
  .clip { display: none; }
  .cell { grid-row: 1 / span 2; }
  .star { grid-row: 1 / span 2; }
}
</style>
