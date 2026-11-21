<script lang="scala">
import skereru.web.*
import org.scalajs.dom
import skereru.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 兄弟のサービス（stui・skawau）との OAuth 連携の状態と、連携する・解除するボタン. 設定の「連携」に置く.
// provider: サービス（stui・skawau）. kind: 何のサービスか（チャット・ファイル共有）. about: 連携すると何ができるか.
// onStatus: 状態を読んだとき・変わったとき.
case class Props(provider: String, kind: String, about: String, env: String, onStatus: LinkStatusDto => Unit = _ => ())

case class State(link: RV[Option[LinkStatusDto]] = RV(None))

def initState(props: Props) = State()

def mounted(s: State, props: Props): Unit = {
  load(s, props)
  // 許可の画面から戻ってきたとき（?integration=…&result=…）
  val q = new dom.URLSearchParams(dom.window.location.search)
  if (Option(q.get("integration")).contains(props.provider)) {
    val name = AppNames.nameOf(props.provider)
    Option(q.get("result")).getOrElse("") match {
      case "connected" => Toasts.show(s"$name と連携しました")
      case "denied"    => Toasts.show("連携は許可されませんでした", error = true)
      case "expired"   => Toasts.show("連携の手続きの期限が切れました。もう一度やり直してください", error = true)
      case _           => Toasts.show(s"$name と連携できませんでした", error = true)
    }
    dom.window.history.replaceState(null, "", "/settings")
  }
}

def load(s: State, props: Props): Unit = Api.get[LinkStatusDto](s"/api/integrations/${props.provider}").foreach { l =>
  s.link := Some(l); props.onStatus(l)
}

def configured(s: State) = s.link.mapC(_.exists(_.configured))
def connected(s: State) = s.link.mapC(_.exists(_.connected))
def url(s: State) = s.link.mapC(_.flatMap(_.url).getOrElse(""))
def lastError(s: State) = s.link.mapC(_.flatMap(_.lastError).getOrElse(""))
def connectUrl(props: Props) = s"/api/integrations/${props.provider}/connect"

def disconnect(s: State, props: Props): Unit =
  Dialog.confirm(s"${AppNames.nameOf(props.provider)} との連携を解除しますか？", "解除") {
    Toasts.report(Api.delete(s"/api/integrations/${props.provider}/connection"))(_ => load(s, props))
  }
</script>

<template>
  <section class="panel">
    <h2>{{ AppNames.of(props.provider) }}（{{ props.kind }}）</h2>
    <p class="muted small">{{ props.about }}</p>
    <p v-if="configured(s).mapC(!_)" class="small">このサーバーでは {{ AppNames.of(props.provider) }} と連携できません（管理者が <code>{{ props.env }}</code> を設定すると使えます）。</p>
    <template v-if="configured(s)">
      <p class="small">{{ AppNames.of(props.provider) }}: <a :href="url(s)" target="_blank" rel="noopener noreferrer">{{ url(s) }}</a>
        ・状態: <b>{{ if (connected(s).v) "連携しています" else "連携していません" }}</b></p>
      <p v-if="lastError(s).mapC(_.nonEmpty)" class="err small">最近の失敗: {{ lastError(s) }}</p>
      <a v-if="connected(s).mapC(!_)" class="btn primary" :href="connectUrl(props)">連携する</a>
      <button v-if="connected(s)" class="danger small" @click="disconnect(s, props)">連携を解除</button>
    </template>
  </section>
</template>

<style scoped>
.err { color: var(--danger); }
</style>
