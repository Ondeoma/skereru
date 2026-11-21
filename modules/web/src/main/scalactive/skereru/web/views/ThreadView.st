<script lang="scala">
import skereru.web.*
import skereru.web.parts.*
import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// スレッド（会話）の表示。最新と未読のメッセージを開き、HTML は sandbox の iframe で表示する
case class Props(id: String)

case class State(thread: RV[Option[ThreadDto]] = RV(None),
                 open: RV[List[UUID]] = RV(Nil),
                 images: RV[List[UUID]] = RV(Nil),
                 details: RV[List[UUID]] = RV(Nil),
                 error: RV[String] = RV(""),
                 invites: RV[Map[String, InviteDto]] = RV(Map.empty),
                 unregister: RV[Option[() => Unit]] = RV(None),
                 saving: RV[Option[(UUID, List[String])]] = RV(None))

def initState() = State()

// ---------------------------------------------------------------- skawau に保存

/** 保存先を選ぶ画面を開く（parts が空なら、そのメールの添付をすべて）. */
def askSave(s: State, m: MessageDetailDto, parts: List[String]): Unit = s.saving := Some((m.summary.id, parts))
def savers(s: State) = s.saving.mapC(_.toList)
def saveTo(s: State, folder: Option[UUID]): Unit = s.saving.v.foreach { (id, parts) =>
  val n = AppNames.nameOf("skawau")
  Toasts.report(Api.post[SkawauSavedDto]("/api/integrations/skawau/save", SkawauSaveReq(id, parts, folder).asJson)) { r =>
    val names = r.saved.map(_.name).mkString("、")
    // 「開く」で skawau の保存先のフォルダを新しいタブで開く
    Toasts.show(s"$n に保存しました: $names", action = r.folderUrl.map(u => ("開く", () => { dom.window.open(u, "_blank", "noopener"); () })), ms = Some(10000))
  }
}

// ---------------------------------------------------------------- 会議の招待

def loadInvites(s: State, t: ThreadDto): Unit =
  t.messages.filter(_.hasInvite).foreach { m =>
    Api.get[InviteDto](s"/api/messages/${m.summary.id}/invite").foreach(i => s.invites := s.invites.v + (m.summary.id.toString -> i))
  }
def invite(s: State, m: MessageDetailDto) = s.invites.mapC(_.get(m.summary.id.toString))
def hasInvite(s: State, m: MessageDetailDto) = s.invites.mapC(_.contains(m.summary.id.toString))
def inv(s: State, m: MessageDetailDto): InviteDto = s.invites.v(m.summary.id.toString)

def when(i: InviteDto): String =
  if (i.allDay) {
    val a = Fmt.date(i.start); val e = new scala.scalajs.js.Date(i.end.toEpochMilli - 86400000.0)
    val b = Fmt.date(Timestamp(e.toISOString()))
    if (a == b) s"$a（終日）" else s"$a 〜 $b（終日）"
  } else s"${Fmt.dateTime(i.start)} 〜 ${Fmt.dateTime(i.end)}"

def statusLabel(p: Option[String]): String = p.getOrElse("") match {
  case "ACCEPTED"  => "参加"
  case "TENTATIVE" => "仮承諾"
  case "DECLINED"  => "辞退"
  case _           => "未回答"
}

def answer(s: State, m: MessageDetailDto, partstat: String): Unit =
  Toasts.report(Api.post[UpdatedDto](s"/api/messages/${m.summary.id}/invite/reply", InviteReplyReq(partstat).asJson)) { _ =>
    Toasts.show(s"「${statusLabel(Some(partstat))}」と返事しました")
    s.invites := s.invites.v.updatedWith(m.summary.id.toString)(_.map(_.copy(myStatus = Some(partstat))))
  }

def addToCalendar(m: MessageDetailDto): Unit =
  Toasts.report(Api.post[CalendarAddedDto](s"/api/messages/${m.summary.id}/invite/calendar")) { c =>
    Toasts.show(s"${AppNames.nameOf("skuaka")} のカレンダーに追加しました")
  }

def statusClass(s: State, m: MessageDetailDto, p: String) = invite(s, m).mapC(i => if (i.flatMap(_.myStatus).contains(p)) "small on" else "small")

def uuid(props: Props): Option[UUID] = scala.util.Try(UUID.fromString(props.id)).toOption

def load(s: State, props: Props, first: Boolean): Unit = uuid(props).foreach { id =>
  Api.thread(id).onComplete {
    case scala.util.Success(t) =>
      s.thread := Some(t)
      loadInvites(s, t)
      pageTitle(if (t.subject.isEmpty) "(件名なし)" else t.subject)
      if (first) {
        // 最後のメッセージと、開いたもの・未読だったものを開く
        val unread = t.messages.filterNot(_.summary.seen).map(_.summary.id)
        s.open := (t.messages.lastOption.map(_.summary.id).toList ++ unread ++ List(id)).distinct
        Store.loadMailboxes()
      }
    case scala.util.Failure(e) => s.error := e.getMessage
  }
}

def mounted(s: State, props: Props): Unit = {
  def last: Option[MessageDetailDto] = s.thread.v.flatMap(_.messages.lastOption)
  s.unregister := Some(Shortcuts.register(
    "u" -> (() => back()), "Escape" -> (() => back()),
    "r" -> (() => last.foreach(reply(_, "reply"))), "a" -> (() => last.foreach(reply(_, "replyAll"))), "f" -> (() => last.foreach(reply(_, "forward"))),
    "e" -> (() => archive(s)), "#" -> (() => remove(s)), "b" -> (() => snooze(s))
  ))
  load(s, props, first = true)
  watch(Store.tick, false)((_, _) => load(s, props, first = false))
}

def unmounted(s: State): Unit = s.unregister.v.foreach(f => f())

def messages(s: State) = s.thread.mapC(_.toList.flatMap(_.messages))
def subject(s: State) = s.thread.mapC(_.map(t => if (t.subject.isEmpty) "(件名なし)" else t.subject).getOrElse(""))
def isOpen(s: State, id: UUID) = s.open.mapC(_.contains(id))
def toggle(s: State, id: UUID): Unit = s.open := (if (s.open.v.contains(id)) s.open.v.filterNot(_ == id) else s.open.v :+ id)
def showDetails(s: State, id: UUID) = s.details.mapC(_.contains(id))
def toggleDetails(s: State, id: UUID): Unit = s.details := (if (s.details.v.contains(id)) s.details.v.filterNot(_ == id) else s.details.v :+ id)

def ids(s: State): List[UUID] = s.thread.v.toList.flatMap(_.messages.map(_.summary.id))

def back(): Unit = if (dom.window.history.length > 1) dom.window.history.back() else Router.go("/mail/inbox")

def act(s: State, r: UpdateMessagesReq, msg: String, leave: Boolean): Unit =
  Toasts.report(Api.update(r)) { _ => Toasts.show(msg); Store.changed(); if (leave) back() }

def archive(s: State): Unit = Store.special(SpecialUse.Archive).foreach(a => act(s, UpdateMessagesReq.of(ids(s)).copy(moveTo = Some(a.id)), "アーカイブしました", true))
def snooze(s: State): Unit = Store.snooze(ids(s))(() => back())
def remove(s: State): Unit = act(s, UpdateMessagesReq.of(ids(s)).copy(delete = Some(true)), "削除しました", true)
def spam(s: State, yes: Boolean): Unit =
  act(s, UpdateMessagesReq.of(ids(s)).copy(spam = Some(yes)), if (yes) "迷惑メールとして報告しました" else "迷惑メールではないと報告しました", true)
def unread(s: State): Unit = s.thread.v.flatMap(_.messages.lastOption).foreach(m => act(s, UpdateMessagesReq.of(List(m.summary.id)).copy(seen = Some(false)), "未読にしました", true))
def move(s: State): Unit = {
  val choices = Store.mailboxes.v.map(b => b.id.toString -> Store.boxLabel(b))
  Dialog.choose("移動先のメールボックス", choices, "移動") { id => act(s, UpdateMessagesReq.of(ids(s)).copy(moveTo = Some(UUID.fromString(id))), "移動しました", true) }
}
def star(s: State, m: MessageDetailDto): Unit =
  Toasts.report(Api.update(UpdateMessagesReq.of(List(m.summary.id)).copy(flagged = Some(!m.summary.flagged))))(_ => load(s, Props(m.summary.id.toString), false))
def isJunk(m: MessageDetailDto): Boolean = Store.mailboxes.v.find(_.id == m.summary.mailboxId).exists(_.specialUse.contains(SpecialUse.Junk))
def anyJunk(s: State) = s.thread.mapC(_.exists(_.messages.exists(isJunk)))

/** 返信・全員に返信・転送の作成画面を開く. */
def reply(m: MessageDetailDto, mode: String): Unit =
  Toasts.report(Api.get[ComposeTemplateDto](s"/api/messages/${m.summary.id}/compose?mode=$mode")) { t =>
    val sig = Store.settings.v.signature.trim
    val sigHtml = if (sig.isEmpty) "" else "<p>-- <br>" + escape(sig).replace("\n", "<br>") + "</p>"
    val sigText = if (sig.isEmpty) "" else s"\n\n-- \n$sig"
    Store.compose(ComposeReq(t.from, t.to, t.cc, Nil, t.subject, sigText + t.quotedText, Some("<p></p>" + sigHtml + t.quotedHtml), Nil,
      if (mode == "forward") None else Some(m.summary.id), if (mode == "forward") Some(m.summary.id) else None, None))
  }

def escape(s: String): String = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

def htmlUrl(s: State, m: MessageDetailDto) = s.images.mapC(l => s"/api/messages/${m.summary.id}/html" + (if (l.contains(m.summary.id)) "?images=1" else ""))
def showImages(s: State, m: MessageDetailDto): Unit = s.images := (s.images.v :+ m.summary.id).distinct
def needImages(s: State, m: MessageDetailDto) = s.images.mapC(l => m.hasRemoteImages && !l.contains(m.summary.id))

/** iframe の高さを中身に合わせる（sandbox でスクリプトは動かないが、同じオリジンなので親から測れる）. */
def fit(e: dom.Event): Unit = {
  val f = e.target.asInstanceOf[dom.HTMLIFrameElement]
  def resize(): Unit = scala.util.Try {
    val d = f.contentDocument
    if (d != null) f.style.height = s"${math.max(60, d.documentElement.scrollHeight) + 4}px"
  }
  resize()
  dom.window.setTimeout(() => resize(), 400)
  dom.window.setTimeout(() => resize(), 1500)
}

def avatar(m: MessageDetailDto): String = s"background: hsl(${Fmt.hue(m.summary.from.map(_.address).getOrElse(""))} 55% 45%)"
def initial(m: MessageDetailDto): String = Fmt.initials(m.summary.from.map(Fmt.addr).getOrElse("?"))
def toLine(m: MessageDetailDto): String = "宛先: " + (m.summary.to ++ m.cc).map(Fmt.addr).mkString(", ")
def authWarn(m: MessageDetailDto): Boolean = m.auth.exists(a => a.dmarc.startsWith("fail") || (a.spf.startsWith("fail") && !a.dkim.startsWith("pass")))
def partUrl(m: MessageDetailDto, a: AttachmentDto): String = s"/api/messages/${m.summary.id}/parts/${a.part}?download=1"
def files(m: MessageDetailDto): List[AttachmentDto] = m.attachments.filterNot(_.inline)
def unsubscribe(m: MessageDetailDto): Option[String] =
  m.listUnsubscribe.flatMap(u => "<(https://[^>]+)>".r.findFirstMatchIn(u).map(_.group(1)))
def cardClass(s: State, m: MessageDetailDto) = s.open.mapC(o => if (o.contains(m.summary.id)) "msg open" else "msg")
</script>

<template>
  <div class="thread">
    <div class="bar">
      <button class="ghost" title="一覧に戻る" @click="back()">←</button>
      <button class="ghost" title="アーカイブ（受信トレイから外して保管します。検索や「アーカイブ」からいつでも見られます）" @click="archive(s)">🗄</button>
      <button class="ghost" title="スヌーズ（指定した日時まで一覧から隠し、その日時に受信トレイへ戻す）" @click="snooze(s)">⏰</button>
      <button v-if="anyJunk(s).mapC(!_)" class="ghost" title="迷惑メールとして報告（迷惑メールに移し、今後の判定に学ばせます）" @click="spam(s, true)">🚫</button>
      <button v-if="anyJunk(s)" class="ghost" title="受信トレイに戻し、今後の判定に学ばせます" @click="spam(s, false)">迷惑メールではない</button>
      <button class="ghost" title="削除（ゴミ箱へ移します）" @click="remove(s)">🗑</button>
      <span class="sep"></span>
      <button class="ghost" title="未読にする" @click="unread(s)">未読にする</button>
      <button class="ghost" title="フォルダへ移動" @click="move(s)">📁 移動</button>
    </div>
    <p v-if="s.error.mapC(_.nonEmpty)" class="error pad">{{ s.error }}</p>
    <h1 class="subject">{{ subject(s) }}</h1>
    <div v-for="m in messages(s)" :key="m.summary.id" :class="cardClass(s, m)">
      <div class="head" @click="toggle(s, m.summary.id)">
        <span class="av" :style="avatar(m)">{{ initial(m) }}</span>
        <div class="grow who">
          <div><b>{{ m.summary.from.map(Fmt.addr).getOrElse("(差出人なし)") }}</b>
            <span class="muted small">&lt;{{ m.summary.from.map(_.address).getOrElse("") }}&gt;</span></div>
          <div v-if="isOpen(s, m.summary.id)" class="muted small to" @click.stop="toggleDetails(s, m.summary.id)">{{ toLine(m) }} ▾</div>
          <div v-else class="muted small preview">{{ m.summary.preview }}</div>
        </div>
        <span class="muted small date">{{ Fmt.dateTime(m.summary.date) }}</span>
        <button :class='if (m.summary.flagged) "ghost star on" else "ghost star"' :title='if (m.summary.flagged) "スターを外す" else "スターを付ける（あとで見返す印）"' @click.stop="star(s, m)">{{ if (m.summary.flagged) "★" else "☆" }}</button>
      </div>
      <template v-if="isOpen(s, m.summary.id)">
        <div v-if="showDetails(s, m.summary.id)" class="details small">
          <div>差出人: {{ m.summary.from.map(Fmt.addrFull).getOrElse("") }}</div>
          <div>宛先: {{ m.summary.to.map(Fmt.addrFull).mkString(", ") }}</div>
          <div v-if="m.cc.nonEmpty">Cc: {{ m.cc.map(Fmt.addrFull).mkString(", ") }}</div>
          <div v-if="m.bcc.nonEmpty">Bcc: {{ m.bcc.map(Fmt.addrFull).mkString(", ") }}</div>
          <div>日時: {{ Fmt.dateTime(m.summary.date) }}</div>
          <div>メールボックス: {{ m.mailboxName }}</div>
          <template v-for="a in m.auth.toList">
            <div>SPF: {{ a.spf }} ／ DKIM: {{ a.dkim }} ／ DMARC: {{ a.dmarc }}</div>
          </template>
          <div v-if="m.spamReport.isDefined" class="muted">迷惑メールの判定: {{ m.summary.spamScore.map(_.toString).getOrElse("") }} {{ m.spamReport.getOrElse("") }}</div>
          <div><a :href='"/api/messages/" + m.summary.id + "/raw"'>元のメッセージをダウンロード（.eml）</a></div>
        </div>
        <div v-if="authWarn(m)" class="warn">⚠️ このメールは差出人のドメインの認証（SPF・DKIM・DMARC）に失敗しています。なりすましの可能性があります。リンクや添付ファイルに注意してください。</div>
        <div v-if="isJunk(m)" class="warn">🚫 迷惑メールと判定されています。リンクを開いたり、返信したりしないでください。</div>
        <div v-if="needImages(s, m)" class="images">
          外部の画像は表示していません（送信者に開封が知られるのを防ぐため）。<button class="small" @click="showImages(s, m)">画像を表示</button>
        </div>
        <div v-if="hasInvite(s, m)" class="invite">
          <div class="row"><span class="cal">📅</span><div class="grow">
            <div class="ititle">{{ inv(s, m).summary }}<span v-if="inv(s, m).method == &quot;CANCEL&quot;" class="cancelled">（取り消されました）</span></div>
            <div class="small">{{ when(inv(s, m)) }}<span v-if="inv(s, m).recurring" class="muted">（繰り返し）</span></div>
            <div v-if="inv(s, m).location.nonEmpty" class="small">📍 {{ inv(s, m).location }}</div>
            <div class="small muted">主催: {{ inv(s, m).organizer.map(Fmt.addrFull).getOrElse("—") }}・出席者 {{ inv(s, m).attendees.size }} 人<span v-if="inv(s, m).me.isDefined">・あなたの返事: {{ statusLabel(inv(s, m).myStatus) }}</span></div>
          </div></div>
          <div class="row ibtns">
            <template v-if="inv(s, m).canReply">
              <button :class="statusClass(s, m, &quot;ACCEPTED&quot;)" title="参加すると主催者に返事を送ります" @click="answer(s, m, &quot;ACCEPTED&quot;)">✔ 参加</button>
              <button :class="statusClass(s, m, &quot;TENTATIVE&quot;)" title="仮承諾と主催者に返事を送ります" @click="answer(s, m, &quot;TENTATIVE&quot;)">？ 仮承諾</button>
              <button :class="statusClass(s, m, &quot;DECLINED&quot;)" title="辞退すると主催者に返事を送ります" @click="answer(s, m, &quot;DECLINED&quot;)">✕ 辞退</button>
            </template>
            <button v-if="inv(s, m).canAddToCalendar" class="small" :title='AppNames.of("skuaka").mapC(_ + " のカレンダーに予定として追加します")' @click="addToCalendar(m)">📅 カレンダーに追加</button>
          </div>
        </div>
        <div class="body">
          <iframe v-if="m.html.isDefined" class="html" :src="htmlUrl(s, m)" sandbox="allow-same-origin allow-popups allow-popups-to-escape-sandbox" referrerpolicy="no-referrer" @load="fit(event)"></iframe>
          <pre v-else class="text">{{ m.text }}</pre>
        </div>
        <div v-if="files(m).nonEmpty" class="files">
          <a v-for="a in files(m)" class="file" :href="partUrl(m, a)" :title="a.filename">
            <span class="fi">📄</span><span class="fname">{{ a.filename }}</span><span class="muted small">{{ Fmt.bytes(a.size.toLong) }}</span>
            <button v-if="Store.skawauReady" type="button" class="ghost small save" :title='AppNames.of("skawau").mapC(_ + " に保存")' @click.prevent.stop="askSave(s, m, List(a.part))">☁️</button>
          </a>
          <button v-if="Store.skawauReady" type="button" class="small" @click="askSave(s, m, Nil)">☁️ すべて {{ AppNames.of("skawau") }} に保存</button>
        </div>
        <div class="actions">
          <button @click="reply(m, &quot;reply&quot;)">↩︎ 返信</button>
          <button @click="reply(m, &quot;replyAll&quot;)">↩︎ 全員に返信</button>
          <button @click="reply(m, &quot;forward&quot;)">↪︎ 転送</button>
          <a v-for="u in unsubscribe(m).toList" class="btn ghost small" :href="u" target="_blank" rel="noopener noreferrer">配信停止</a>
        </div>
      </template>
    </div>
    <SkawauPicker v-for="_ in savers(s)" :folderMode="true" :onClose="_ => s.saving := None" :onFolder="(f: Option[UUID]) => saveTo(s, f)" />
  </div>
</template>

<style scoped>
.file .save { padding: 0 .2rem; }
.invite { border: 1px solid var(--line); border-left: 4px solid var(--accent); border-radius: 8px; padding: .7rem .9rem; margin: .6rem 0; background: var(--side); }
.invite .cal { font-size: 1.6rem; }
.invite .ititle { font-weight: 700; }
.invite .cancelled { color: var(--danger); font-weight: 400; margin-left: .3rem; }
.ibtns { margin-top: .5rem; gap: .4rem; flex-wrap: wrap; }
.ibtns .on { background: var(--accent-weak); color: var(--accent); border-color: var(--accent); }
.thread { padding-bottom: 6rem; }
.bar { position: sticky; top: var(--header); z-index: 50; display: flex; align-items: center; gap: .2rem; padding: .35rem .8rem; background: var(--bg);
  border-bottom: 1px solid var(--line); min-height: 46px; }
.sep { width: 1px; height: 20px; background: var(--line); margin: 0 .3rem; }
.pad { padding: 1rem; }
.subject { font-size: 1.35rem; font-weight: 500; margin: 1.2rem 1.4rem .8rem 4.2rem; }
.msg { margin: 0 1rem 0 1rem; border-bottom: 1px solid var(--line); }
.head { display: flex; align-items: flex-start; gap: .8rem; padding: .8rem .4rem; cursor: pointer; }
.av { width: 40px; height: 40px; border-radius: 50%; color: #fff; display: inline-flex; align-items: center; justify-content: center; font-weight: 700; flex: none; }
.who { min-width: 0; }
.preview { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.to { cursor: pointer; }
.date { white-space: nowrap; padding-top: .15rem; }
.star { padding: 0 .2rem; font-size: 1.05rem; color: var(--muted); }
.star.on { color: #f4b400; }
.details { margin: 0 0 .6rem 3.6rem; padding: .6rem .8rem; border: 1px solid var(--line); border-radius: 8px; background: var(--side); line-height: 1.7; word-break: break-all; }
.warn { margin: 0 .4rem .6rem 3.6rem; padding: .55rem .8rem; border-radius: 8px; background: color-mix(in srgb, var(--danger) 12%, transparent); font-size: .86rem; }
.images { margin: 0 .4rem .6rem 3.6rem; padding: .45rem .8rem; border-radius: 8px; background: var(--side); font-size: .84rem; display: flex; gap: .6rem; align-items: center; flex-wrap: wrap; }
.body { margin: 0 .4rem 0 3.6rem; }
.html { width: 100%; border: none; min-height: 60px; background: #fff; border-radius: 6px; }
.text { white-space: pre-wrap; word-break: break-word; font-family: inherit; font-size: .92rem; margin: 0; line-height: 1.65; }
.files { display: flex; flex-wrap: wrap; gap: .6rem; margin: .9rem .4rem 0 3.6rem; }
.file { display: flex; align-items: center; gap: .4rem; border: 1px solid var(--line); border-radius: 8px; padding: .45rem .7rem; color: var(--fg); max-width: 260px; }
.file:hover { background: var(--hover); text-decoration: none; }
.fname { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: .86rem; }
.actions { display: flex; gap: .5rem; flex-wrap: wrap; margin: 1rem .4rem 1.2rem 3.6rem; }
.actions button { border-radius: 18px; padding: .4rem 1rem; }
@media (max-width: 720px) {
  .subject { margin: .8rem; }
  .msg { margin: 0; }
  .body, .files, .actions, .details, .warn, .images { margin-left: .4rem; }
}
</style>
