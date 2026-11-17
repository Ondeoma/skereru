<script lang="scala">
import skereru.web.*
import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*

import java.util.UUID
import scala.scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// メールの作成画面（右下に開く。最小化・下書きの保存・添付・宛先の候補）
case class Props(draft: Draft)

case class State(from: RV[String],
                 to: RV[String],
                 cc: RV[String],
                 bcc: RV[String],
                 subject: RV[String],
                 showCc: RV[Boolean],
                 uploads: RV[List[UploadDto]] = RV(Nil),
                 draftId: RV[Option[UUID]],
                 sending: RV[Boolean] = RV(false),
                 uploading: RV[Int] = RV(0),
                 dirty: RV[Boolean] = RV(false),
                 suggest: RV[List[ContactDto]] = RV(Nil),
                 suggestFor: RV[String] = RV(""),
                 el: ElementRef[dom.HTMLElement] = ElementRef(),
                 editor: RV[Option[EditorHandle]] = RV(None),
                 timer: RV[Option[Int]] = RV(None),
                 full: RV[Boolean] = RV(false))

def initState(props: Props) = {
  val i = props.draft.init
  State(RV(if (i.from.nonEmpty) i.from else Store.sendable.headOption.getOrElse("")), RV(i.to.mkString(", ")), RV(i.cc.mkString(", ")),
    RV(i.bcc.mkString(", ")), RV(i.subject), RV(i.cc.nonEmpty || i.bcc.nonEmpty), draftId = RV(i.draftId), uploads = RV(props.draft.uploads))
}

def textToHtml(t: String): String =
  t.split("\n", -1).map(l => "<p>" + l.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;") + "</p>").mkString

def mounted(s: State, props: Props): Unit = {
  s.el.foreach { el =>
    val i = props.draft.init
    val h = Editor.create(el, i.html.getOrElse(textToHtml(i.text)), "本文", () => s.dirty := true, files => addFiles(s, files))
    s.editor := Some(h)
    if (i.to.nonEmpty) h.focus()
  }
  // 1分ごとに下書きを保存する
  s.timer := Some(dom.window.setInterval(() => if (s.dirty.v && !s.sending.v && hasContent(s)) saveDraft(s, props, quiet = true), 60000))
}

def unmounted(s: State): Unit = {
  s.timer.v.foreach(dom.window.clearInterval)
  s.editor.v.foreach(_.destroy())
}

def hasContent(s: State): Boolean =
  s.to.v.trim.nonEmpty || s.subject.v.trim.nonEmpty || s.editor.v.exists(e => !e.isEmpty()) || s.uploads.v.nonEmpty

def req(s: State, props: Props): ComposeReq = {
  val i = props.draft.init
  def list(v: String) = if (v.trim.isEmpty) Nil else List(v.trim)
  ComposeReq(s.from.v, list(s.to.v), list(s.cc.v), list(s.bcc.v), s.subject.v, s.editor.v.map(_.getText()).getOrElse(""),
    s.editor.v.map(_.getHTML()), s.uploads.v.map(_.id), i.inReplyTo, i.forwardOf, s.draftId.v)
}

def send(s: State, props: Props, at: Option[Timestamp] = None): Unit =
  if (s.to.v.trim.isEmpty && s.cc.v.trim.isEmpty && s.bcc.v.trim.isEmpty) Dialog.alert("宛先を入力してください")
  else if (s.uploading.v > 0) Dialog.alert("添付ファイルのアップロードが終わるまでお待ちください")
  else if (s.subject.v.trim.isEmpty) Dialog.confirm("件名がありません。このまま送信しますか？", "送信", danger = false)(doSend(s, props, at))
  else doSend(s, props, at)

/** 日時を指定して送る. */
def sendLater(s: State, props: Props): Unit = When.pick("送信する日時")(t => send(s, props, Some(t)))

/** 送る（すぐに送らず、取り消しの猶予の後か、指定した日時にサーバーが送る）. */
def doSend(s: State, props: Props, at: Option[Timestamp]): Unit = {
  s.sending := true
  Api.post[ScheduledDto]("/api/schedule", ScheduleReq(req(s, props), at).asJson).onComplete {
    case Success(d) =>
      Store.closeDraft(props.draft.seq)
      val wait = d.sendAt.toEpochMilli - js.Date.now()
      val undo = Some(() => Store.unschedule(d.id))
      if (at.isDefined) Store.toast(s"${When.label(d.sendAt.toDate)} に送信します", undo = undo, undoLabel = "取り消す")
      else if (wait <= 500) Store.toast("送信しました")
      else Store.toast("送信しています…", undo = undo, undoLabel = "取り消す", ms = wait.toInt)
      // 送り終わったころに一覧を取り直す
      dom.window.setTimeout(() => Store.changed(), math.max(1000, wait + 3000))
      Store.changed()
    case Failure(e) =>
      s.sending := false
      Store.error(e)
  }
}

def saveDraft(s: State, props: Props, quiet: Boolean): Unit =
  Api.post[DraftSavedDto]("/api/drafts", req(s, props).asJson).onComplete {
    case Success(d) =>
      s.draftId := Some(d.draftId); s.dirty := false
      if (!quiet) Store.toast("下書きを保存しました")
      Store.changed()
    case Failure(e) => if (!quiet) Store.error(e)
  }

/** 閉じる（書きかけなら下書きに保存してから）. */
def close(s: State, props: Props): Unit = {
  if (s.dirty.v && hasContent(s)) saveDraft(s, props, quiet = false)
  Store.closeDraft(props.draft.seq)
}

def discard(s: State, props: Props): Unit = {
  s.draftId.v.foreach(id => Api.update(UpdateMessagesReq.of(List(id)).copy(delete = Some(true))).foreach(_ => Store.changed()))
  Store.closeDraft(props.draft.seq)
}

/** 📎 ボタンで選んだファイルを添付する. */
def attach(s: State, e: dom.Event): Unit = {
  val input = e.target.asInstanceOf[dom.HTMLInputElement]
  val files = (0 until input.files.length).map(input.files(_)).toList
  input.value = ""
  addFiles(s, files)
}

/** ファイルをアップロードして添付に加える（📎 ボタン・本文へのドロップ）. */
def addFiles(s: State, files: List[dom.File]): Unit =
  files.foreach { f =>
    s.uploading := s.uploading.v + 1
    Api.upload(f).onComplete { r =>
      s.uploading := s.uploading.v - 1
      r match {
        case Success(u) => s.uploads := s.uploads.v :+ u; s.dirty := true
        case Failure(e) => Store.error(e)
      }
    }
  }

def removeUpload(s: State, u: UploadDto): Unit = { s.uploads := s.uploads.v.filterNot(_.id == u.id); s.dirty := true }

/** 宛先の欄の入力（最後の宛先で候補を出す）. */
def typed(s: State, field: String, rv: RV[String], v: String): Unit = {
  rv := v
  s.dirty := true
  val last = v.split("[,;]").lastOption.getOrElse("").trim
  if (last.length >= 1 && !last.contains("<")) {
    s.suggestFor := field
    Api.contacts(last).foreach(cs => if (s.suggestFor.v == field) s.suggest := cs.take(6))
  } else s.suggest := Nil
}

def pick(s: State, rv: RV[String], c: ContactDto): Unit = {
  val parts = rv.v.split("[,;]").toList.dropRight(1).map(_.trim).filter(_.nonEmpty)
  val item = MailText.quoted(c.name, c.address)
  rv := (parts :+ item).mkString(", ") + ", "
  s.suggest := Nil
}

def hideSuggest(s: State): Unit = dom.window.setTimeout(() => s.suggest := Nil, 200)
def suggestOn(s: State, field: String) = computed { s.suggestFor.v == field && s.suggest.v.nonEmpty }

def title(s: State) = s.subject.mapC(t => if (t.trim.isEmpty) "新規メッセージ" else t)
def cls(s: State, props: Props) = s.full.mapC(f => "composer" + (if (props.draft.minimized) " min" else "") + (if (f) " full" else ""))
def fromOptions: List[Opt] = Store.sendable.map(a => Opt(a, a))
def multipleFrom: Boolean = Store.sendable.size > 1
def isForward(props: Props): Boolean = props.draft.init.forwardOf.isDefined
def sendLabel(s: State) = s.sending.mapC(b => if (b) "送信中…" else "送信")
def uploadingNow(s: State) = s.uploading.mapC(_ > 0)

def key(s: State, props: Props, e: dom.Event): Unit = {
  val k = e.asInstanceOf[dom.KeyboardEvent]
  if ((k.ctrlKey || k.metaKey) && k.key == "Enter") { k.preventDefault(); send(s, props) }
}
</script>

<template>
  <div :class="cls(s, props)" @keydown="key(s, props, event)">
    <div class="titlebar" @click="Store.toggleDraft(props.draft.seq)">
      <span class="grow t">{{ title(s) }}</span>
      <button class="ghost" title="小さくする" @click.stop="Store.toggleDraft(props.draft.seq)">▁</button>
      <button class="ghost" title="画面いっぱいに広げる" @click.stop="s.full := !s.full.v">⤢</button>
      <button class="ghost" title="閉じる（下書きを保存）" @click.stop="close(s, props)">✕</button>
    </div>
    <div class="content">
      <div v-if="multipleFrom" class="line"><span class="lbl">差出人</span><Combo :bind="Some(s.from)" :options="fromOptions" :small="true" /></div>
      <div class="line rel">
        <span class="lbl">宛先</span>
        <input type="text" :value="s.to" @input="typed(s, &quot;to&quot;, s.to, Dom.value(event))" @blur="hideSuggest(s)" autocomplete="off">
        <button v-if="s.showCc.mapC(!_)" class="ghost small" @click="s.showCc := true">Cc Bcc</button>
        <div v-if="suggestOn(s, &quot;to&quot;)" class="sugg">
          <div v-for="c in s.suggest" class="opt" @mousedown.prevent="pick(s, s.to, c)"><b>{{ c.name.getOrElse("") }}</b> <span class="muted">{{ c.address }}</span></div>
        </div>
      </div>
      <template v-if="s.showCc">
        <div class="line rel">
          <span class="lbl">Cc</span>
          <input type="text" :value="s.cc" @input="typed(s, &quot;cc&quot;, s.cc, Dom.value(event))" @blur="hideSuggest(s)" autocomplete="off">
          <div v-if="suggestOn(s, &quot;cc&quot;)" class="sugg">
            <div v-for="c in s.suggest" class="opt" @mousedown.prevent="pick(s, s.cc, c)"><b>{{ c.name.getOrElse("") }}</b> <span class="muted">{{ c.address }}</span></div>
          </div>
        </div>
        <div class="line rel">
          <span class="lbl">Bcc</span>
          <input type="text" :value="s.bcc" @input="typed(s, &quot;bcc&quot;, s.bcc, Dom.value(event))" @blur="hideSuggest(s)" autocomplete="off">
          <div v-if="suggestOn(s, &quot;bcc&quot;)" class="sugg">
            <div v-for="c in s.suggest" class="opt" @mousedown.prevent="pick(s, s.bcc, c)"><b>{{ c.name.getOrElse("") }}</b> <span class="muted">{{ c.address }}</span></div>
          </div>
        </div>
      </template>
      <div class="line"><input type="text" v-model="s.subject" placeholder="件名" class="subject" @input="s.dirty := true"></div>
      <div ref="s.el" class="editor"></div>
      <p v-if="isForward(props)" class="muted small note">元のメールの添付ファイルも一緒に送ります。</p>
      <div v-if="s.uploads.mapC(_.nonEmpty)" class="files">
        <span v-for="u in s.uploads" class="file">📎 {{ u.filename }} <span class="muted">{{ Fmt.bytes(u.size) }}</span>
          <button class="ghost small" title="この添付を外す" @click="removeUpload(s, u)">✕</button></span>
      </div>
      <p v-if="uploadingNow(s)" class="muted small note">アップロード中…</p>
    </div>
    <div class="foot">
      <button class="primary send" :disabled="s.sending" title="送信（送ったあと、少しの間は取り消せます）" @click="send(s, props)">{{ sendLabel(s) }}</button>
      <button class="ghost" :disabled="s.sending" title="日時を指定して送信" @click="sendLater(s, props)">⏰</button>
      <label class="btn ghost" title="ファイルを添付（本文にドロップしても添付できます）">📎<input type="file" multiple class="hidden" @change="attach(s, event)"></label>
      <span class="grow"></span>
      <button class="ghost" title="下書きを保存" @click="saveDraft(s, props, false)">💾</button>
      <button class="ghost" title="破棄（このメールと下書きを削除します）" @click="discard(s, props)">🗑</button>
    </div>
  </div>
</template>

<style scoped>
.composer { pointer-events: auto; width: 560px; max-width: calc(100vw - 1rem); height: 560px; max-height: calc(100vh - var(--header) - 1rem);
  display: flex; flex-direction: column; background: var(--surface); border-radius: 12px 12px 0 0; box-shadow: 0 8px 10px 1px rgba(0,0,0,.14), 0 3px 14px 2px rgba(0,0,0,.12), 0 5px 5px -3px rgba(0,0,0,.2); }
.composer.full { position: fixed; inset: 6vh 8vw; width: auto; height: auto; max-width: none; border-radius: 12px; }
.composer.min { height: auto; width: 300px; }
.composer.min .content, .composer.min .foot { display: none; }
.titlebar { display: flex; align-items: center; gap: .2rem; padding: .35rem .5rem .35rem .9rem; background: var(--side); border-radius: 12px 12px 0 0; cursor: pointer; }
.titlebar button { padding: 0 .35rem; }
.t { font-weight: 600; font-size: .88rem; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.content { flex: 1; overflow-y: auto; display: flex; flex-direction: column; padding: 0 .9rem; }
.line { display: flex; align-items: center; gap: .4rem; border-bottom: 1px solid var(--line); }
.line input { border: none; box-shadow: none !important; flex: 1; padding: .5rem 0; background: transparent; }
.lbl { color: var(--muted); font-size: .85rem; min-width: 3em; }
.rel { position: relative; }
.sugg { position: absolute; top: 100%; left: 3em; right: 0; z-index: 30; background: var(--surface); border: 1px solid var(--line); border-radius: 8px; box-shadow: var(--shadow); padding: 4px; }
.opt { padding: .35rem .55rem; border-radius: 6px; cursor: pointer; font-size: .86rem; }
.opt:hover { background: var(--accent-weak); }
.subject { font-size: .95rem; }
.editor { flex: 1; min-height: 160px; padding: .6rem 0; }
.note { margin: .3rem 0; }
.files { display: flex; flex-wrap: wrap; gap: .4rem; padding: .4rem 0; }
.file { font-size: .82rem; border: 1px solid var(--line); border-radius: 6px; padding: .15rem .3rem .15rem .5rem; display: inline-flex; align-items: center; gap: .3rem; }
.foot { display: flex; align-items: center; gap: .3rem; padding: .6rem .9rem; }
.send { border-radius: 18px; padding: .5rem 1.4rem; font-weight: 600; }
@media (max-width: 720px) {
  .composer:not(.min) { position: fixed; inset: var(--header) 0 0 0; width: auto; height: auto; max-height: none; border-radius: 0; }
}
</style>
