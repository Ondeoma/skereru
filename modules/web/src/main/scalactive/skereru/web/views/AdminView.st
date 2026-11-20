<script lang="scala">
import skereru.web.*
import skereru.web.parts.*
import io.circe.Json
import io.circe.syntax.*
import skereru.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 管理: 概要・ドメイン（DNS の案内）・ユーザー・アドレス（別名・転送）・エージェント専用アカウント・送信待ち・監査ログ
case class State(tab: RV[String] = RV("overview"),
                 stats: RV[Option[StatsDto]] = RV(None),
                 domains: RV[List[DomainDto]] = RV(Nil),
                 newDomain: RV[String] = RV(""),
                 autoProv: RV[Boolean] = RV(false),
                 dns: RV[Option[(DomainDto, List[DnsRecordDto])]] = RV(None),
                 users: RV[List[AdminUserDto]] = RV(Nil),
                 userQ: RV[String] = RV(""),
                 uName: RV[String] = RV(""),
                 uAddr: RV[String] = RV(""),
                 uEmail: RV[String] = RV(""),
                 uQuota: RV[String] = RV(""),
                 addrs: RV[List[AddressDto]] = RV(Nil),
                 aAddr: RV[String] = RV(""),
                 aUser: RV[String] = RV(""),
                 aFwd: RV[String] = RV(""),
                 aSend: RV[Boolean] = RV(true),
                 agents: RV[List[AgentDto]] = RV(Nil),
                 gName: RV[String] = RV(""),
                 gAddr: RV[String] = RV(""),
                 keyFor: RV[Option[AgentDto]] = RV(None),
                 kName: RV[String] = RV("skakapo"),
                 kScopes: RV[List[String]] = RV(List("read", "write")),
                 kDays: RV[String] = RV(""),
                 createdKey: RV[Option[(String, String)]] = RV(None),
                 queue: RV[List[QueueItemDto]] = RV(Nil),
                 qStatus: RV[String] = RV("queued"),
                 audit: RV[List[AuditDto]] = RV(Nil),
                 dmarc: RV[List[DmarcReportDto]] = RV(Nil),
                 dmarcOpen: RV[Option[(DmarcReportDto, List[DmarcRowDto])]] = RV(None))

def initState() = State()

def mounted(s: State): Unit = { pageTitle("管理"); loadAll(s) }

def loadAll(s: State): Unit = {
  Api.get[StatsDto]("/api/admin/stats").foreach(x => s.stats := Some(x))
  loadDomains(s); loadUsers(s); loadAddrs(s); loadAgents(s); loadQueue(s)
  Api.get[List[AuditDto]]("/api/admin/audit?limit=200").foreach(s.audit := _)
  Api.get[List[DmarcReportDto]]("/api/admin/dmarc").foreach(s.dmarc := _)
}

// ---------------------------------------------------------------- DMARC の集計レポート

def openDmarc(s: State, r: DmarcReportDto): Unit =
  Api.get[List[DmarcRowDto]](s"/api/admin/dmarc/${r.id}/rows").foreach(rows => s.dmarcOpen := Some(r -> rows.sortBy(-_.count)))
def passRate(r: DmarcReportDto): String = if (r.total == 0) "—" else s"${r.passing * 100 / r.total}%"
def dmarcClass(r: DmarcReportDto): String = if (r.total > 0 && r.passing < r.total) "warn" else ""
def rowClass(x: DmarcRowDto): String = if (x.dkim != "pass" && x.spf != "pass") "warn" else ""
def openReport(s: State) = s.dmarcOpen.mapC(_.map(_._1))
def openRows(s: State) = s.dmarcOpen.mapC(_.map(_._2).getOrElse(Nil))
def openTitle(s: State) = s.dmarcOpen.mapC(_.map(o => s"${o._1.orgName}（${o._1.domain}・${Fmt.date(o._1.begin)}〜${Fmt.date(o._1.end)}）").getOrElse(""))

val tabs: List[(String, String)] = List("overview" -> "概要", "domains" -> "ドメイン", "users" -> "ユーザー", "addresses" -> "アドレス・別名",
  "agents" -> "エージェント", "queue" -> "送信待ち", "dmarc" -> "DMARC", "audit" -> "監査ログ")
def tabClass(s: State, t: String) = s.tab.mapC(x => if (x == t) "active" else "")
def on(s: State, t: String) = s.tab.mapC(_ == t)

// ---------------------------------------------------------------- ドメイン

def loadDomains(s: State): Unit = Api.get[List[DomainDto]]("/api/admin/domains").foreach(s.domains := _)
def addDomain(s: State): Unit =
  Toasts.report(Api.post[Json]("/api/admin/domains", CreateDomainReq(s.newDomain.v.trim, s.autoProv.v).asJson)) { _ =>
    s.newDomain := ""; loadDomains(s); Toasts.show("ドメインを追加しました。DNS のレコードを設定してください")
  }
def showDns(s: State, d: DomainDto): Unit =
  Toasts.report(Api.get[List[DnsRecordDto]](s"/api/admin/domains/${d.id}/dns"))(r => s.dns := Some((d, r)))
def dnsList(s: State) = s.dns.mapC(_.toList)
def patchDomain(s: State, d: DomainDto, r: UpdateDomainReq): Unit =
  Toasts.report(Api.patchUnit(s"/api/admin/domains/${d.id}", r.asJson))(_ => loadDomains(s))
def deleteDomain(s: State, d: DomainDto): Unit = Dialog.confirm(s"ドメイン ${d.name} を削除しますか？", "削除") {
  Toasts.report(Api.delete(s"/api/admin/domains/${d.id}"))(_ => loadDomains(s))
}
def rotate(s: State, d: DomainDto): Unit =
  Dialog.confirm(s"${d.name} の DKIM の鍵を作り直しますか？新しいセレクターの TXT レコードを DNS に追加してください（古いレコードは数日後に消してください）。", "作り直す", danger = false) {
    Toasts.report(Api.postUnit(s"/api/admin/domains/${d.id}/rotate-dkim")) { _ => loadDomains(s); showDns(s, d) }
  }
def okMark(r: DnsRecordDto): String = r.ok match {
  case Some(true)  => "✅"
  case Some(false) => "⚠️"
  case None        => "—"
}
def catchAllName(s: State, d: DomainDto): String = d.catchAll.flatMap(id => s.users.v.find(_.id == id)).map(_.name).getOrElse("なし")
def setCatchAll(s: State, d: DomainDto): Unit =
  Dialog.choose(s"${d.name} の存在しないアドレス宛てのメールを受け取るユーザー（キャッチオール）", ("" -> "なし（受け取らない）") :: s.users.v.map(u => u.id.toString -> u.name), "設定") { v =>
    patchDomain(s, d, UpdateDomainReq(None, None, Some(Option(v).filter(_.nonEmpty).map(UUID.fromString))))
  }

// ---------------------------------------------------------------- ユーザー

def loadUsers(s: State): Unit = Api.get[List[AdminUserDto]]("/api/admin/users" + Api.query("q" -> Some(s.userQ.v))).foreach(s.users := _)
def addUser(s: State): Unit =
  Toasts.report(Api.post[Json]("/api/admin/users", CreateUserReq(s.uName.v.trim, s.uAddr.v.trim, Option(s.uEmail.v.trim).filter(_.nonEmpty), s.uQuota.v.toIntOption).asJson)) { _ =>
    s.uName := ""; s.uAddr := ""; s.uEmail := ""; s.uQuota := ""; loadUsers(s); loadAddrs(s); Toasts.show("ユーザーを作りました")
  }
def patchUser(s: State, u: AdminUserDto, r: UpdateUserReq): Unit = Toasts.report(Api.patchUnit(s"/api/admin/users/${u.id}", r.asJson))(_ => loadUsers(s))
def quota(s: State, u: AdminUserDto): Unit = Dialog.prompt(s"${u.name} の容量（MB）", (u.quotaBytes / 1024 / 1024).toString, "変更") { v =>
  v.trim.toIntOption.foreach(q => patchUser(s, u, UpdateUserReq(None, None, None, Some(q))))
}
def deleteUser(s: State, u: AdminUserDto): Unit = Dialog.confirm(s"ユーザー ${u.name} と、そのメールをすべて削除しますか？（元に戻せません）", "削除") {
  Toasts.report(Api.delete(s"/api/admin/users/${u.id}")) { _ => loadUsers(s); loadAddrs(s) }
}
def usage(u: AdminUserDto): String = s"${Fmt.bytes(u.usedBytes)} / ${Fmt.bytes(u.quotaBytes)}"

// ---------------------------------------------------------------- アドレス

def loadAddrs(s: State): Unit = Api.get[List[AddressDto]]("/api/admin/addresses").foreach(s.addrs := _)
def addAddr(s: State): Unit = {
  val fwd = s.aFwd.v.split("[,\\s]+").map(_.trim).filter(_.nonEmpty).toList
  Toasts.report(Api.post[Json]("/api/admin/addresses", AddressReq(s.aAddr.v.trim, Option(s.aUser.v).filter(_.nonEmpty).map(UUID.fromString), fwd, s.aSend.v).asJson)) { _ =>
    s.aAddr := ""; s.aFwd := ""; loadAddrs(s); Toasts.show("アドレスを追加しました")
  }
}
def editFwd(s: State, a: AddressDto): Unit = Dialog.prompt(s"${a.address} の転送先（カンマ区切り）", a.forwardTo.mkString(", "), "保存") { v =>
  val fwd = v.split("[,\\s]+").map(_.trim).filter(_.nonEmpty).toList
  Toasts.report(Api.putUnit(s"/api/admin/addresses/${a.id}", AddressReq(a.address, a.userId, fwd, a.canSend).asJson))(_ => loadAddrs(s))
}
def deleteAddr(s: State, a: AddressDto): Unit = Dialog.confirm(s"アドレス ${a.address} を削除しますか？", "削除") {
  Toasts.report(Api.delete(s"/api/admin/addresses/${a.id}"))(_ => loadAddrs(s))
}
def userChoices(s: State) = s.users.mapC(us => Opt("", "（ユーザーなし・転送だけ）") :: us.map(u => Opt(u.id.toString, u.name, hint = u.addresses.headOption.getOrElse(""))))

// ---------------------------------------------------------------- エージェント専用アカウント

def loadAgents(s: State): Unit = Api.get[List[AgentDto]]("/api/admin/agents").foreach(s.agents := _)
def addAgent(s: State): Unit =
  Toasts.report(Api.post[AgentDto]("/api/admin/agents", CreateAgentReq(s.gName.v.trim, Option(s.gAddr.v.trim).filter(_.nonEmpty)).asJson)) { _ =>
    s.gName := ""; s.gAddr := ""; loadAgents(s); loadUsers(s); loadAddrs(s); Toasts.show("エージェント専用アカウントを作りました")
  }
def patchAgent(s: State, a: AgentDto, r: UpdateAgentReq): Unit = Toasts.report(Api.patch[AgentDto](s"/api/admin/agents/${a.id}", r.asJson))(_ => loadAgents(s))
def renameAgent(s: State, a: AgentDto): Unit = Dialog.prompt("エージェント専用アカウントの名前", a.name, "変更") { v =>
  patchAgent(s, a, UpdateAgentReq(Some(v), None))
}
def openKey(s: State, a: AgentDto): Unit = { s.keyFor := Some(a); s.createdKey := None }
def keyForList(s: State) = s.keyFor.mapC(_.toList)
def issueKey(s: State, a: AgentDto): Unit =
  Toasts.report(Api.post[CreatedApiKeyDto](s"/api/admin/agents/${a.id}/keys", CreateApiKeyReq(s.kName.v, s.kScopes.v, s.kDays.v.toIntOption).asJson)) { r =>
    s.keyFor := None; s.createdKey := Some(a.name -> r.key); loadAgents(s)
  }
def revokeAgentKey(s: State, a: AgentDto, k: ApiKeyDto): Unit =
  Dialog.confirm(s"${a.name} の APIキー「${k.name}」を失効させますか？このキーを使っているエージェントは使えなくなります。", "失効") {
    Toasts.report(Api.delete(s"/api/admin/agents/${a.id}/keys/${k.id}"))(_ => loadAgents(s))
  }
def createdKeyText(s: State) = s.createdKey.mapC(_.map(_._2).getOrElse(""))
def createdKeyFor(s: State) = s.createdKey.mapC(_.map(_._1).getOrElse(""))

// ---------------------------------------------------------------- 送信待ち

def loadQueue(s: State): Unit = Api.get[List[QueueItemDto]]("/api/admin/queue" + Api.query("status" -> Some(s.qStatus.v))).foreach(s.queue := _)
def retry(s: State, q: QueueItemDto): Unit = Toasts.report(Api.postUnit(s"/api/admin/queue/${q.id}/retry"))(_ => loadQueue(s))
def cancel(s: State, q: QueueItemDto): Unit = Toasts.report(Api.postUnit(s"/api/admin/queue/${q.id}/cancel"))(_ => loadQueue(s))
def stat(s: State, f: StatsDto => String) = s.stats.mapC(_.map(f).getOrElse("…"))
</script>

<template>
  <div class="page">
    <h1>管理</h1>
    <nav class="tabs">
      <button v-for="t in tabs" :class="tabClass(s, t._1)" @click="s.tab := t._1">{{ t._2 }}</button>
    </nav>

    <template v-if="on(s, &quot;overview&quot;)">
      <div class="tiles">
        <div class="tile"><div class="num">{{ stat(s, _.users.toString) }}</div><div class="muted small">ユーザー</div></div>
        <div class="tile"><div class="num">{{ stat(s, _.domains.toString) }}</div><div class="muted small">ドメイン</div></div>
        <div class="tile"><div class="num">{{ stat(s, _.messages.toString) }}</div><div class="muted small">メッセージ（{{ stat(s, x => Fmt.bytes(x.bytes)) }}）</div></div>
        <div class="tile"><div class="num">{{ stat(s, _.received24h.toString) }}</div><div class="muted small">受信（24時間）</div></div>
        <div class="tile"><div class="num">{{ stat(s, _.junk24h.toString) }}</div><div class="muted small">迷惑メール（24時間）</div></div>
        <div class="tile"><div class="num">{{ stat(s, _.rejected24h.toString) }}</div><div class="muted small">受信を拒否（24時間）</div></div>
        <div class="tile"><div class="num">{{ stat(s, _.sent24h.toString) }}</div><div class="muted small">送信（24時間）</div></div>
        <div class="tile"><div class="num">{{ stat(s, x => s"${x.queued} / ${x.failed}") }}</div><div class="muted small">送信待ち / 失敗</div></div>
      </div>
    </template>

    <template v-if="on(s, &quot;domains&quot;)">
      <section class="panel">
        <h2>ドメインを追加</h2>
        <form class="row" @submit.prevent="addDomain(s)">
          <input type="text" v-model="s.newDomain" placeholder="example.com" class="grow">
          <label class="small"><input type="checkbox" v-model="s.autoProv"> IdP のメールアドレスでログインした人のメールボックスを自動で作る</label>
          <button type="submit" class="primary">追加</button>
        </form>
      </section>
      <div class="table-wrap">
        <table class="list">
          <thead><tr><th>ドメイン</th><th>有効</th><th>自動作成</th><th>キャッチオール</th><th>アドレス</th><th>DKIM</th><th></th></tr></thead>
          <tbody>
            <tr v-for="d in s.domains" :key="d.id">
              <td><b>{{ d.name }}</b></td>
              <td><input type="checkbox" :checked="d.enabled" @change="patchDomain(s, d, UpdateDomainReq(Some(Dom.checked(event)), None, None))"></td>
              <td><input type="checkbox" :checked="d.autoProvision" @change="patchDomain(s, d, UpdateDomainReq(None, Some(Dom.checked(event)), None))"></td>
              <td><button class="ghost small" @click="setCatchAll(s, d)">{{ catchAllName(s, d) }}</button></td>
              <td>{{ d.addressCount }}</td>
              <td><code>{{ d.dkimSelector }}</code></td>
              <td class="nowrap">
                <button class="small primary" @click="showDns(s, d)">DNS の設定</button>
                <button class="small" @click="rotate(s, d)">鍵を作り直す</button>
                <button class="small danger" @click="deleteDomain(s, d)">削除</button>
              </td>
            </tr>
            <tr v-if="s.domains.mapC(_.isEmpty)"><td colspan="7" class="muted">ドメインはありません。まずドメインを追加してください</td></tr>
          </tbody>
        </table>
      </div>
      <Modal v-for="x in dnsList(s)" :onClose="_ => s.dns := None" :xwide="true">
        <h2>{{ x._1.name }} の DNS レコード</h2>
        <p class="muted small">ドメインの DNS に次のレコードを設定してください。✅ は今の設定が合っているもの、⚠️ は見つからないか違うものです。</p>
        <div class="table-wrap">
          <table class="list dns">
            <thead><tr><th></th><th>種類</th><th>名前</th><th>値</th><th>用途</th></tr></thead>
            <tbody>
              <tr v-for="r in x._2">
                <td>{{ okMark(r) }}</td><td>{{ r.kind }}</td><td><code>{{ r.name }}</code></td>
                <td><code class="val" @click="Dom.copy(r.value)" title="クリックでコピー">{{ r.value }}</code>
                  <div v-if="r.actual.isDefined &amp;&amp; !r.ok.contains(true)" class="muted small">今の値: {{ r.actual.getOrElse("") }}</div></td>
                <td class="small">{{ r.purpose }}</td>
              </tr>
            </tbody>
          </table>
        </div>
        <div class="actions"><button @click="showDns(s, x._1)">再確認</button><button class="primary" @click="s.dns := None">閉じる</button></div>
      </Modal>
    </template>

    <template v-if="on(s, &quot;users&quot;)">
      <section class="panel">
        <h2>ユーザーを作る</h2>
        <form @submit.prevent="addUser(s)">
          <div class="row">
            <input type="text" v-model="s.uName" placeholder="名前" class="grow">
            <input type="email" v-model="s.uAddr" placeholder="メールアドレス（例: taro@example.com）" class="grow">
          </div>
          <div class="row">
            <input type="email" v-model="s.uEmail" placeholder="IdP のメールアドレス（省略時は上と同じ）" class="grow">
            <input type="number" v-model="s.uQuota" placeholder="容量（MB）" class="q">
            <button type="submit" class="primary">作成</button>
          </div>
        </form>
        <p class="muted small">IdP でこのメールアドレスのユーザーが最初にログインしたときに結び付きます。</p>
      </section>
      <div class="row filter"><input type="search" v-model="s.userQ" placeholder="名前・アドレスで絞り込み" @input="loadUsers(s)" class="grow"></div>
      <div class="table-wrap">
        <table class="list">
          <thead><tr><th>名前</th><th>アドレス</th><th>容量</th><th>管理者</th><th>無効</th><th>最終ログイン</th><th></th></tr></thead>
          <tbody>
            <tr v-for="u in s.users" :key="u.id">
              <td><b>{{ u.name }}</b><div class="muted small">{{ u.email.getOrElse("") }}{{ if (u.linked) "" else "（未ログイン）" }}</div></td>
              <td class="small">{{ u.addresses.mkString(", ") }}</td>
              <td class="small nowrap"><button class="ghost small" @click="quota(s, u)">{{ usage(u) }}</button></td>
              <td><input type="checkbox" :checked="u.isAdmin" @change="patchUser(s, u, UpdateUserReq(None, Some(Dom.checked(event)), None, None))"></td>
              <td><input type="checkbox" :checked="u.disabled" @change="patchUser(s, u, UpdateUserReq(None, None, Some(Dom.checked(event)), None))"></td>
              <td class="small">{{ u.lastLoginAt.map(Fmt.ago).getOrElse("—") }}</td>
              <td><button class="danger small" @click="deleteUser(s, u)">削除</button></td>
            </tr>
          </tbody>
        </table>
      </div>
    </template>

    <template v-if="on(s, &quot;addresses&quot;)">
      <section class="panel">
        <h2>別名・転送を追加</h2>
        <form @submit.prevent="addAddr(s)">
          <div class="row">
            <input type="email" v-model="s.aAddr" placeholder="アドレス（例: info@example.com）" class="grow">
            <Combo :bind="Some(s.aUser)" :live="Some(userChoices(s))" placeholder="配送先のユーザー" />
          </div>
          <div class="row">
            <input type="text" v-model="s.aFwd" placeholder="転送先（カンマ区切り。このサーバーのアドレスも可）" class="grow">
            <label class="small"><input type="checkbox" v-model="s.aSend"> 差出人に使える</label>
            <button type="submit" class="primary">追加</button>
          </div>
        </form>
        <p class="muted small">複数の人に配るアドレス（グループ）は、転送先にこのサーバーのアドレスを並べて作れます。</p>
      </section>
      <div class="table-wrap">
        <table class="list">
          <thead><tr><th>アドレス</th><th>配送先</th><th>転送先</th><th>送信</th><th></th></tr></thead>
          <tbody>
            <tr v-for="a in s.addrs" :key="a.id">
              <td><b>{{ a.address }}</b> <span v-if="a.isPrimary" class="badge">主</span></td>
              <td>{{ a.userName.getOrElse("—") }}</td>
              <td class="small"><button class="ghost small" @click="editFwd(s, a)">{{ if (a.forwardTo.isEmpty) "なし" else a.forwardTo.mkString(", ") }}</button></td>
              <td>{{ if (a.canSend) "○" else "" }}</td>
              <td><button class="danger small" @click="deleteAddr(s, a)">削除</button></td>
            </tr>
          </tbody>
        </table>
      </div>
    </template>

    <template v-if="on(s, &quot;agents&quot;)">
      <section class="panel">
        <h2>エージェント専用アカウント</h2>
        <p class="muted small">skakapo などの外の AI エージェントが、このサービスの MCP（<code>/mcp</code>）と REST API を使うためのアカウントです。
          人はこのアカウントでログインできず、管理者が発行した APIキーでだけ使えます。</p>
        <p class="muted small">エージェントは<b>このアカウント自身の権限</b>で動きます。見られるのは、このアカウントのメールボックス（このアカウントに配送されるアドレス宛てのメール）だけです。
          ほかの人のメールを扱わせたいときは、ふつうの方法で権限を与えてください（「アドレス・別名」で配送先をこのアカウントにする・転送先に加える、など）。
          送信には、差出人に使えるアドレスと send のスコープが必要です。</p>
        <p class="muted small">エージェントが行った送信・整理などは、このアカウントが行ったものとして記録されます（監査ログの経路は mcp・api）。</p>
        <form @submit.prevent="addAgent(s)">
          <div class="row">
            <input type="text" v-model="s.gName" placeholder="名前（例: skakapo）" maxlength="60" class="grow">
            <input type="email" v-model="s.gAddr" placeholder="送信に使うアドレス（省略可。例: agent@example.com）" class="grow">
            <button type="submit" class="primary">作成</button>
          </div>
        </form>
      </section>
      <div v-if="s.createdKey.mapC(_.isDefined)" class="panel created">
        <p><b>{{ createdKeyFor(s) }} の APIキーを発行しました。</b></p>
        <div class="row"><code class="grow keytext">{{ createdKeyText(s) }}</code><button class="small" @click="Dom.copy(createdKeyText(s).v)">コピー</button></div>
        <p class="small">この画面を閉じると二度と表示されません。skakapo の管理画面の「サービス」→ このサービス →「エージェント専用アカウント」に貼り付けてください。</p>
        <button class="small" @click="s.createdKey := None">閉じる</button>
      </div>
      <div class="table-wrap">
        <table class="list">
          <thead><tr><th>名前</th><th>アドレス</th><th>APIキー</th><th>無効</th><th></th></tr></thead>
          <tbody>
            <tr v-for="a in s.agents" :key="a.id">
              <td><b>{{ a.name }}</b><div class="muted small">作成 {{ Fmt.date(a.createdAt) }}</div></td>
              <td class="small">{{ if (a.addresses.isEmpty) "—" else a.addresses.mkString(", ") }}</td>
              <td class="small">
                <div v-for="k in a.keys" :key="k.id" class="keyrow">
                  <code>{{ k.prefix }}…</code> {{ k.name }}（{{ k.scopes.mkString(", ") }}）
                  <span class="muted">{{ k.expiresAt.map(x => "期限 " + Fmt.date(x)).getOrElse("無期限") }}・{{ k.lastUsedAt.map(Fmt.ago).getOrElse("未使用") }}</span>
                  <button class="danger small" @click="revokeAgentKey(s, a, k)">失効</button>
                </div>
                <span v-if="a.keys.isEmpty" class="muted">なし</span>
              </td>
              <td><input type="checkbox" :checked="a.disabled" @change="patchAgent(s, a, UpdateAgentReq(None, Some(Dom.checked(event))))"></td>
              <td class="nowrap">
                <button class="small primary" @click="openKey(s, a)">キーを発行</button>
                <button class="small" @click="renameAgent(s, a)">名前を変更</button>
              </td>
            </tr>
            <tr v-if="s.agents.mapC(_.isEmpty)"><td colspan="5" class="muted">エージェント専用アカウントはありません</td></tr>
          </tbody>
        </table>
      </div>
      <Modal v-for="a in keyForList(s)" :onClose="_ => s.keyFor := None">
        <h2>{{ a.name }} の APIキーを発行</h2>
        <form @submit.prevent="issueKey(s, a)">
          <div class="row">
            <input type="text" v-model="s.kName" placeholder="用途（例: skakapo）" class="grow">
            <Combo :bind="Some(s.kDays)" :options='Opt.of(List("" -> "無期限", "30" -> "30日", "90" -> "90日", "365" -> "1年"))' />
          </div>
          <div class="row">
            <span class="small muted">スコープ:</span>
            <label v-for="sc in ApiScope.all.filterNot(_ == ApiScope.Admin)" class="small"><input type="checkbox" :value="sc" v-model="s.kScopes"> {{ ApiScope.label(sc) }}（{{ sc }}）</label>
          </div>
          <p class="muted small">エージェント専用アカウントのキーに管理（admin）のスコープは付けられません。</p>
          <div class="actions"><button type="button" @click="s.keyFor := None">やめる</button><button type="submit" class="primary">発行</button></div>
        </form>
      </Modal>
    </template>

    <template v-if="on(s, &quot;queue&quot;)">
      <div class="row filter">
        <Combo :bind="Some(s.qStatus)" :options='Opt.of(List("queued" -> "送信待ち", "failed" -> "失敗", "sent" -> "送信済み"))' :onChange="_ => loadQueue(s)" />
        <button @click="loadQueue(s)">更新</button>
      </div>
      <div class="table-wrap">
        <table class="list">
          <thead><tr><th>作成</th><th>差出人</th><th>宛先</th><th>試行</th><th>次の試行</th><th>エラー</th><th></th></tr></thead>
          <tbody>
            <tr v-for="q in s.queue" :key="q.id">
              <td class="small">{{ Fmt.dateTime(q.createdAt) }}</td><td class="small">{{ if (q.sender.isEmpty) "<>" else q.sender }}</td><td class="small">{{ q.recipient }}</td>
              <td>{{ q.attempts }}</td><td class="small">{{ Fmt.dateTime(q.nextAttemptAt) }}</td><td class="small err">{{ q.lastError.getOrElse("") }}</td>
              <td class="nowrap"><button class="small" @click="retry(s, q)">再送</button><button v-if="q.status == &quot;queued&quot;" class="small danger" @click="cancel(s, q)">取消</button></td>
            </tr>
            <tr v-if="s.queue.mapC(_.isEmpty)"><td colspan="7" class="muted">ありません</td></tr>
          </tbody>
        </table>
      </div>
    </template>

    <template v-if="on(s, &quot;dmarc&quot;)">
      <p class="muted small">ほかのメールサーバー（Google・Microsoft など）から届いた DMARC の集計レポートです。自分のドメインを名乗るメールが、どこから・どれだけ送られ、
        認証に通ったかが分かります。通っていない送信元が多いときは、なりすましか、SPF・DKIM の設定漏れ（自社の別のサービスなど）です。
        レポートは <code>dmarc-reports@&lt;ドメイン&gt;</code> で受け取ります（「ドメイン → DNS の設定」の DMARC のレコードの rua）。</p>
      <div class="table-wrap">
        <table class="list">
          <thead><tr><th>期間</th><th>ドメイン</th><th>報告元</th><th>通数</th><th>認証の合格</th><th>ポリシー</th><th></th></tr></thead>
          <tbody>
            <tr v-for="r in s.dmarc" :key="r.id.toString" :class="dmarcClass(r)">
              <td class="small nowrap">{{ Fmt.date(r.begin) }}〜{{ Fmt.date(r.end) }}</td><td>{{ r.domain }}</td>
              <td>{{ r.orgName }} <span v-if="!r.authenticated" class="muted small" title="報告のメールが SPF・DKIM で認証されていません（偽の報告の可能性）">（未認証）</span></td>
              <td>{{ r.total }}</td><td>{{ passRate(r) }}</td><td><code>{{ r.policy }}</code></td>
              <td><button class="small" @click="openDmarc(s, r)">詳しく</button></td>
            </tr>
            <tr v-if="s.dmarc.mapC(_.isEmpty)"><td colspan="7" class="muted">レポートはまだ届いていません（届くのは DMARC のレコードを設定した翌日ごろからです）</td></tr>
          </tbody>
        </table>
      </div>
      <section v-if="openReport(s).mapC(_.isDefined)" class="panel">
        <div class="row"><h2 class="grow">{{ openTitle(s) }}</h2><button class="small" @click="s.dmarcOpen := None">閉じる</button></div>
        <div class="table-wrap">
          <table class="list">
            <thead><tr><th>送信元の IP</th><th>通数</th><th>DKIM</th><th>SPF</th><th>扱い</th><th>From</th><th>認証したドメイン</th></tr></thead>
            <tbody>
              <tr v-for="x in openRows(s)" :key="x.sourceIp + x.headerFrom" :class="rowClass(x)">
                <td><code>{{ x.sourceIp }}</code></td><td>{{ x.count }}</td><td>{{ x.dkim }}</td><td>{{ x.spf }}</td><td>{{ x.disposition }}</td>
                <td class="small">{{ x.headerFrom }}</td><td class="small">{{ (x.dkimDomains ++ x.spfDomains).distinct.mkString(", ") }}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </section>
    </template>

    <template v-if="on(s, &quot;audit&quot;)">
      <div class="table-wrap">
        <table class="list">
          <thead><tr><th>日時</th><th>ユーザー</th><th>経路</th><th>操作</th><th>対象</th><th>IP</th></tr></thead>
          <tbody>
            <tr v-for="a in s.audit" :key="a.id.toString">
              <td class="small nowrap">{{ Fmt.dateTime(a.at) }}</td><td>{{ a.user.getOrElse("—") }}</td><td>{{ a.via }}</td><td><code>{{ a.action }}</code></td>
              <td class="small">{{ a.target.getOrElse("") }}</td><td class="small">{{ a.ip.getOrElse("") }}</td>
            </tr>
          </tbody>
        </table>
      </div>
    </template>
  </div>
</template>

<style scoped>
.tiles { display: grid; grid-template-columns: repeat(auto-fill, minmax(180px, 1fr)); gap: .8rem; }
.tile { border: 1px solid var(--line); border-radius: 10px; padding: 1rem; }
.num { font-size: 1.6rem; font-weight: 700; }
.filter { margin: .8rem 0; }
.q { width: 8rem; }
.dns code { word-break: break-all; }
tr.warn td { color: var(--danger); }
.val { cursor: copy; background: var(--side); padding: .1rem .3rem; border-radius: 4px; }
.keytext { background: var(--side); padding: .3rem .5rem; border-radius: 5px; word-break: break-all; font-size: .8rem; }
.keyrow { margin: .15rem 0; }
.err { color: var(--danger); max-width: 28rem; word-break: break-word; }
</style>
