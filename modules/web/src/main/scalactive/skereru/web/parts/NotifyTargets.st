<script lang="scala">
import skereru.web.*
import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 受信の通知先（stui・受信 Webhook・署名付きの Webhook）. 設定の「連携」に、stui との連携（LinkPanel）の下に置く.
// onChange: 通知先の一覧が変わったとき（振り分けの規則の「通知先に知らせる」の選択肢に使う）.
case class Props(onChange: List[NotifyTargetDto] => Unit = _ => ())

case class State(link: RV[Option[LinkStatusDto]] = RV(None),
                 targets: RV[List[NotifyTargetDto]] = RV(Nil),
                 channels: RV[List[StuiChannelDto]] = RV(Nil),
                 testing: RV[Option[UUID]] = RV(None),
                 // 編集中の通知先（値ごとに RV に持つ. まとめて1つの値にすると、入力のたびにモーダルが作り直されてちらつく）
                 open: RV[Boolean] = RV(false),
                 editId: RV[Option[UUID]] = RV(None),
                 name: RV[String] = RV(""),
                 kind: RV[String] = RV(NotifyKind.Stui),
                 channelId: RV[String] = RV(""),
                 url: RV[String] = RV(""),
                 allInbox: RV[Boolean] = RV(true),
                 enabled: RV[Boolean] = RV(true),
                 secret: RV[String] = RV(""))

def initState(props: Props) = State()

def mounted(s: State, props: Props): Unit = {
  loadLink(s)
  load(s, props)
}

def loadLink(s: State): Unit = Api.get[LinkStatusDto]("/api/integrations/stui").foreach { l =>
  s.link := Some(l)
  if (l.connected) Api.get[List[StuiChannelDto]]("/api/integrations/stui/channels").foreach(s.channels := _)
}
def load(s: State, props: Props): Unit = Api.get[List[NotifyTargetDto]]("/api/me/notify-targets").foreach { l => s.targets := l; props.onChange(l) }

def connected(s: State) = s.link.mapC(_.exists(_.connected))
def stuiName = AppNames.of("stui")

// ---------------------------------------------------------------- 通知先

def dest(t: NotifyTargetDto): String = t.kind match {
  case NotifyKind.Stui => t.channelId.fold("自分への DM")(_ => "#" + t.channelName.getOrElse("チャンネル"))
  case _               => t.url.getOrElse("")
}
def scope(t: NotifyTargetDto): String = if (t.allInbox) "受信トレイの新着すべて" else "振り分けの規則で選んだときだけ"

/** 編集を開く（None なら新しく作る）. */
def edit(s: State, t: Option[NotifyTargetDto]): Unit = {
  s.editId := t.map(_.id)
  s.name := t.fold("")(_.name)
  s.kind := t.fold(if (s.link.v.exists(_.configured)) NotifyKind.Stui else NotifyKind.ChatHook)(_.kind)
  s.channelId := t.flatMap(_.channelId).getOrElse("")
  s.url := t.flatMap(_.url).getOrElse("")
  s.allInbox := t.forall(_.allInbox)
  s.enabled := t.forall(_.enabled)
  s.secret := t.flatMap(_.secret).getOrElse("")
  s.open := true
}
def isNew(s: State) = s.editId.mapC(_.isEmpty)
def isStui(s: State) = s.kind.mapC(_ == NotifyKind.Stui)
def isChatHook(s: State) = s.kind.mapC(_ == NotifyKind.ChatHook)
def isWebhook(s: State) = s.kind.mapC(_ == NotifyKind.Webhook)
def kindLabel(s: State) = s.kind.mapC(NotifyKind.label)
/** 編集中の通知先の署名の鍵（作った後だけ表示する）. */
def secret(s: State) = s.secret

val kindOpts: List[Opt] = NotifyKind.all.map(k => Opt(k, NotifyKind.label(k)))
def channelOpts(s: State) = s.channels.mapC(cs => Opt("", "自分への DM") :: cs.map(c => Opt(c.id, (if (c.kind == "private") "🔒 " else "#") + c.name)))

def req(s: State): NotifyTargetReq = {
  val ch = Option(s.channelId.v).filter(_.nonEmpty)
  NotifyTargetReq(s.name.v, s.kind.v, ch, ch.flatMap(id => s.channels.v.find(_.id == id).map(_.name)),
    Option(s.url.v.trim).filter(_.nonEmpty), s.allInbox.v, s.enabled.v)
}

def save(s: State, props: Props): Unit = {
  val id = s.editId.v
  val f = id match {
    case Some(x) => Api.put[NotifyTargetDto](s"/api/me/notify-targets/$x", req(s).asJson)
    case None    => Api.post[NotifyTargetDto]("/api/me/notify-targets", req(s).asJson)
  }
  Toasts.report(f) { t =>
    load(s, props)
    // 作った Webhook は、署名の鍵を見せるために開いたままにする
    if (id.isEmpty && t.kind == NotifyKind.Webhook) edit(s, Some(t)) else s.open := false
    Toasts.show("通知先を保存しました")
  }
}

def remove(s: State, props: Props, t: NotifyTargetDto): Unit =
  Dialog.confirm(s"通知先「${t.name}」を削除しますか？（この通知先を選んでいる振り分けの規則からは、何も送られなくなります）", "削除") {
    Toasts.report(Api.delete(s"/api/me/notify-targets/${t.id}"))(_ => load(s, props))
  }

def test(s: State, props: Props, t: NotifyTargetDto): Unit = {
  s.testing := Some(t.id)
  val f = Api.send(dom.HttpMethod.POST, s"/api/me/notify-targets/${t.id}/test")
  f.onComplete(_ => s.testing := None)
  Toasts.report(f) { _ => Toasts.show(s"「${t.name}」に試しの通知を送りました"); load(s, props) }
}
def testingId(s: State) = s.testing.mapC(_.map(_.toString).getOrElse(""))
</script>

<template>
  <div>
    <section class="panel">
      <div class="row"><h2 class="grow">通知先</h2><button class="primary small" @click="edit(s, None)">＋ 通知先を追加</button></div>
      <p class="muted small">新しいメールの差出人・件名・リンクを送ります（本文は送りません）。「受信トレイの新着すべて」にしない通知先は、「振り分け」の規則の処理「通知先に知らせる」で選んだときだけ送ります。</p>
      <table class="list">
        <thead><tr><th>名前</th><th>送り先</th><th>送るもの</th><th></th></tr></thead>
        <tbody>
          <tr v-for="t in s.targets" :key="t.id">
            <td><b>{{ t.name }}</b> <span v-if="!t.enabled" class="badge">無効</span>
              <div class="muted small">{{ NotifyKind.label(t.kind) }}</div>
              <div v-if="t.lastError.isDefined" class="err small">送れませんでした: {{ t.lastError.getOrElse("") }}</div></td>
            <td class="small dest">{{ dest(t) }}</td>
            <td class="small">{{ scope(t) }}</td>
            <td class="nowrap">
              <button class="small" :disabled="testingId(s).mapC(_ == t.id.toString)" @click="test(s, props, t)">試す</button>
              <button class="small" @click="edit(s, Some(t))">編集</button>
              <button class="danger small" @click="remove(s, props, t)">削除</button>
            </td>
          </tr>
          <tr v-if="s.targets.mapC(_.isEmpty)"><td colspan="4" class="muted">通知先はありません</td></tr>
        </tbody>
      </table>
    </section>

    <Modal v-if="s.open" :onClose="_ => s.open := false" :wide="true">
      <h2>通知先</h2>
      <label class="field"><span>名前</span><input type="text" v-model="s.name" placeholder="例: stui の DM" maxlength="100"></label>
      <div class="field"><span>種類</span>
        <Combo v-if="isNew(s)" :bind="Some(s.kind)" :options="kindOpts" />
        <span v-if="isNew(s).mapC(!_)">{{ kindLabel(s) }}</span></div>

      <template v-if="isStui(s)">
        <p v-if="connected(s).mapC(!_)" class="err small">先に上の「連携する」で {{ stuiName }} と連携してください。</p>
        <div class="field"><span>投稿先</span>
          <Combo :bind="Some(s.channelId)" :live="Some(channelOpts(s))" /></div>
        <p class="muted small">チャンネルには、{{ stuiName }} のボット（{{ AppNames.self }}）として投稿します。チャンネルの全員に差出人と件名が見えます。</p>
      </template>

      <template v-if="isStui(s).mapC(!_)">
        <label class="field"><span>送り先の URL</span><input type="url" v-model="s.url" placeholder="https://…"></label>
        <p v-if="isChatHook(s)" class="muted small">{{ stuiName }}・Slack・Mattermost の受信 Webhook の URL です（{{ stuiName }} ではチャンネルの設定の「連携」で作れます）。<code>{"text": "…", "username": "skereru"}</code> を送ります。</p>
        <template v-if="isWebhook(s)">
          <p class="muted small">イベント <code>mail.received</code> の JSON（<code>id</code>・<code>receivedAt</code>・<code>mailbox</code>・<code>from</code>・<code>subject</code>・<code>url</code>）を POST します。
            ヘッダー <code>X-Skereru-Signature</code> は、本文の HMAC-SHA256（<code>sha256=…</code>）です。下の署名の鍵で確かめてください。</p>
          <div v-if="secret(s).mapC(_.nonEmpty)" class="field"><span>署名の鍵</span>
            <div class="row"><code class="keytext grow">{{ secret(s) }}</code><button type="button" class="small" @click="Dom.copy(secret(s).v)">コピー</button></div></div>
          <p v-if="isNew(s)" class="muted small">署名の鍵は、保存すると作ります。</p>
        </template>
      </template>

      <label class="row"><input type="checkbox" v-model="s.allInbox"> 受信トレイの新着をすべて知らせる（迷惑メール・既読にする規則のものを除く）</label>
      <label class="row"><input type="checkbox" v-model="s.enabled"> 有効</label>
      <div class="actions">
        <button @click="s.open := false">閉じる</button>
        <button class="primary" @click="save(s, props)">保存</button>
      </div>
    </Modal>
  </div>
</template>

<style scoped>
.dest { word-break: break-all; }
.nowrap { white-space: nowrap; }
.err { color: var(--danger); }
.keytext { background: var(--side); padding: .3rem .5rem; border-radius: 5px; word-break: break-all; font-size: .8rem; }
</style>
