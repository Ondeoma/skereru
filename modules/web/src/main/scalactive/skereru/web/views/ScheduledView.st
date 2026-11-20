<script lang="scala">
import skereru.web.*
import org.scalajs.dom
import skereru.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 予約済みの送信（送る前なら取り消して、作成画面に戻せる）
case class State(list: RV[List[ScheduledDto]] = RV(Nil), loaded: RV[Boolean] = RV(false))

def initState() = State()

def load(s: State): Unit = Api.get[List[ScheduledDto]]("/api/scheduled").foreach { l => s.list := l; s.loaded := true }

def mounted(s: State): Unit = {
  pageTitle("予約済み")
  load(s)
  // 送られたものを一覧から消すため、ときどき取り直す（片付けると自動で止まる）
  useInterval(15000)(load(s))
  ()
}

def cancel(s: State, d: ScheduledDto): Unit = { Store.unschedule(d.id); dom.window.setTimeout(() => load(s), 800) }

def when(d: ScheduledDto): String = When.label(d.sendAt.toDate)
def status(d: ScheduledDto): String = d.status match {
  case "queued"  => "送信待ち"
  case "sending" => "送信中"
  case "failed"  => "失敗"
  case other     => other
}
def isEmpty(s: State) = s.loaded.mapC(l => l && s.list.v.isEmpty)
</script>

<template>
  <div class="page">
    <h1>予約済み</h1>
    <p class="muted small">予約した送信です。送る前なら「取り消して編集」で作成画面に戻せます。失敗したものは、取り消して編集し直してください。</p>
    <table class="list">
      <thead><tr><th>送信日時</th><th>宛先</th><th>件名</th><th>状態</th><th></th></tr></thead>
      <tbody>
        <tr v-for="d in s.list" :key="d.id">
          <td class="small nowrap">{{ when(d) }}</td>
          <td class="small">{{ d.to.mkString(", ") }}</td>
          <td>{{ if (d.subject.isEmpty) "（件名なし）" else d.subject }}</td>
          <td class="small">{{ status(d) }}<div v-if="d.error.isDefined" class="small err">{{ d.error.getOrElse("") }}</div></td>
          <td><button v-if="d.status != &quot;sending&quot;" class="small" title="送信をやめて、作成画面に戻す" @click="cancel(s, d)">取り消して編集</button></td>
        </tr>
        <tr v-if="isEmpty(s)"><td colspan="5" class="muted">予約した送信はありません。作成画面の ⏰ で、日時を指定して送れます。</td></tr>
      </tbody>
    </table>
  </div>
</template>

<style scoped>
.err { color: var(--danger); }
</style>
