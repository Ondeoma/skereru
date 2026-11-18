<script lang="scala">
import skereru.web.*
import skereru.web.parts.*
import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*

import java.util.UUID
import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

// 設定: 一般（アイコン・名前・表示など）・自動返信・振り分け・迷惑メール・アドレス・メールソフト・APIキー
case class State(tab: RV[String] = RV("general"),
                 name: RV[String] = RV(""),
                 signature: RV[String] = RV(""),
                 conversation: RV[Boolean] = RV(true),
                 images: RV[Boolean] = RV(false),
                 pageSize: RV[String] = RV("50"),
                 undoSecs: RV[String] = RV("10"),
                 vacOn: RV[Boolean] = RV(false),
                 vacSubject: RV[String] = RV(""),
                 vacBody: RV[String] = RV(""),
                 vacStart: RV[String] = RV(""),
                 vacEnd: RV[String] = RV(""),
                 vacContacts: RV[Boolean] = RV(false),
                 allow: RV[String] = RV(""),
                 block: RV[String] = RV(""),
                 rules: RV[List[FilterRuleDto]] = RV(Nil),
                 editing: RV[Option[(Option[UUID], FilterRuleReq)]] = RV(None),
                 passwords: RV[List[AppPasswordDto]] = RV(Nil),
                 pwName: RV[String] = RV(""),
                 createdPw: RV[Option[CreatedAppPasswordDto]] = RV(None),
                 keys: RV[List[ApiKeyDto]] = RV(Nil),
                 keyName: RV[String] = RV(""),
                 scopes: RV[List[String]] = RV(List("read")),
                 days: RV[String] = RV("90"),
                 createdKey: RV[Option[String]] = RV(None),
                 grants: RV[List[OAuthGrantDto]] = RV(Nil),
                 imports: RV[List[ImportJobDto]] = RV(Nil),
                 imHost: RV[String] = RV("imap.gmail.com"),
                 imPort: RV[String] = RV("993"),
                 imUser: RV[String] = RV(""),
                 imPass: RV[String] = RV(""),
                 imBusy: RV[Boolean] = RV(false),
                 fileRunning: RV[Boolean] = RV(false),
                 fileCancel: RV[Boolean] = RV(false),
                 fileProgress: RV[String] = RV(""),
                 dropping: RV[Boolean] = RV(false),
                 timer: RV[Option[Int]] = RV(None),
                 exportBox: RV[String] = RV(""),
                 integ: RV[Option[IntegrationsDto]] = RV(None),
                 skToken: RV[String] = RV(""))

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("設定")
  Api.get[IntegrationsDto]("/api/me/integrations").foreach(i => s.integ := Some(i))
  // 取り込みの進み具合（取り込みのタブを開いていて、進行中のものがあるときだけ確かめる）
  loadImports(s)
  s.timer := Some(dom.window.setInterval(() => if (s.tab.v == "import" && s.imports.v.exists(j => active(j))) loadImports(s), 3000))
  Store.me.v.foreach(m => s.name := m.name)
  Api.get[MailSettingsDto]("/api/me/settings").foreach { x =>
    Store.settings := x
    s.signature := x.signature; s.conversation := x.conversationView; s.images := x.alwaysShowImages; s.pageSize := x.pageSize.toString; s.undoSecs := x.undoSendSeconds.toString
    s.vacOn := x.vacation.enabled; s.vacSubject := x.vacation.subject; s.vacBody := x.vacation.body
    s.vacStart := x.vacation.startDate.getOrElse(""); s.vacEnd := x.vacation.endDate.getOrElse(""); s.vacContacts := x.vacation.contactsOnly
    s.allow := x.allowSenders.mkString("\n"); s.block := x.blockSenders.mkString("\n")
  }
  loadRules(s)
  loadPasswords(s)
  loadKeys(s)
}

val tabs: List[(String, String)] = List("general" -> "一般", "vacation" -> "自動返信", "filters" -> "振り分け", "spam" -> "迷惑メール",
  "addresses" -> "アドレス", "clients" -> "メールソフト", "import" -> "取り込み・書き出し", "integrations" -> "連携", "api" -> "APIキー・MCP")
def tabClass(s: State, t: String) = s.tab.mapC(x => if (x == t) "active" else "")
def on(s: State, t: String) = s.tab.mapC(_ == t)

def lines(v: String): List[String] = v.split("\n").map(_.trim).filter(_.nonEmpty).toList
def opt(v: String): Option[String] = Option(v.trim).filter(_.nonEmpty)

def save(s: State): Unit = {
  val x = MailSettingsDto(s.signature.v, VacationDto(s.vacOn.v, s.vacSubject.v, s.vacBody.v, opt(s.vacStart.v), opt(s.vacEnd.v), s.vacContacts.v),
    s.images.v, lines(s.allow.v), lines(s.block.v), s.conversation.v, s.pageSize.v.toIntOption.getOrElse(50),
    s.undoSecs.v.toIntOption.getOrElse(10))
  Store.report(Api.put[MailSettingsDto]("/api/me/settings", x.asJson)) { saved => Store.settings := saved; Store.toast("保存しました"); Store.changed() }
}

// ---------------------------------------------------------------- アイコン

val AvatarPx = 256

def myName = Store.me.mapC(_.map(_.name).getOrElse(""))
def myPicture = Store.me.mapC(_.flatMap(_.avatarUrl))
/** このサービスに保存したアイコンがあるか（なければ IdP の画像か、名前の1文字目）. */
def uploaded = Store.me.mapC(_.exists(_.avatarUrl.exists(_.startsWith("/api/users/"))))

/** 画像の中央を正方形に切り抜き、256×256 にする（WebP. 対応していないブラウザでは PNG）. */
def square(file: dom.File): Future[dom.Blob] = {
  js.Dynamic.global.createImageBitmap(file).asInstanceOf[js.Promise[js.Dynamic]].toFuture.flatMap { img =>
    val (w, h) = (img.width.asInstanceOf[Double], img.height.asInstanceOf[Double])
    val side = math.min(w, h)
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    canvas.width = AvatarPx; canvas.height = AvatarPx
    val ctx = canvas.getContext("2d").asInstanceOf[js.Dynamic]
    ctx.imageSmoothingQuality = "high"
    ctx.drawImage(img, (w - side) / 2, (h - side) / 2, side, side, 0, 0, AvatarPx, AvatarPx)
    val p = Promise[dom.Blob]()
    canvas.asInstanceOf[js.Dynamic].toBlob((b: dom.Blob) =>
      if (b == null) p.failure(new Exception("画像を作れませんでした")) else p.success(b), "image/webp", 0.9)
    p.future
  }
}

def pickAvatar(e: dom.Event): Unit = {
  val input = e.target.asInstanceOf[dom.HTMLInputElement]
  Option(input.files).filter(_.length > 0).map(_(0)).foreach { f =>
    input.value = "" // 同じファイルを選び直せるように
    if (!f.`type`.startsWith("image/")) Store.toast("画像のファイルを選んでください", error = true)
    else Store.report(square(f).flatMap(b => Api.putBlob("/api/me/avatar", b))) { _ => Store.toast("アイコンを保存しました"); Store.reloadMe() }
  }
}

def removeAvatar(): Unit =
  Dialog.confirm("保存したアイコンを消しますか？（ログインの仕組みから受け取った画像か、名前の1文字目の表示に戻ります）", "消す") {
    Store.report(Api.delete("/api/me/avatar")) { _ => Store.toast("アイコンを消しました"); Store.reloadMe() }
  }

def saveName(s: State): Unit =
  Store.report(Api.patchUnit("/api/me", UpdateMeReq(Some(s.name.v), None).asJson)) { _ => Store.toast("保存しました"); Store.reloadMe() }

// ---------------------------------------------------------------- 振り分け

def loadRules(s: State): Unit = Api.get[List[FilterRuleDto]]("/api/filters").foreach(s.rules := _)

def newRule(s: State): Unit = s.editing := Some((None, FilterRuleReq("", true, true, List(RuleConditionDto(RuleField.From, RuleOp.Contains, "", None)),
  List(RuleActionDto(RuleAction.Move, Store.mailboxes.v.find(_.specialUse.isEmpty).map(_.name))))))
def editRule(s: State, r: FilterRuleDto): Unit = s.editing := Some((Some(r.id), FilterRuleReq(r.name, r.enabled, r.matchAll, r.conditions, r.actions)))
def form(s: State) = s.editing.mapC(_.toList.map(_._2))
def upd(s: State)(f: FilterRuleReq => FilterRuleReq): Unit = s.editing := s.editing.v.map((id, r) => (id, f(r)))
def setCond(s: State, i: Int)(f: RuleConditionDto => RuleConditionDto): Unit = upd(s)(r => r.copy(conditions = r.conditions.zipWithIndex.map((c, k) => if (k == i) f(c) else c)))
def setAct(s: State, i: Int)(f: RuleActionDto => RuleActionDto): Unit = upd(s)(r => r.copy(actions = r.actions.zipWithIndex.map((a, k) => if (k == i) f(a) else a)))
def conds(s: State) = s.editing.mapC(_.toList.flatMap(_._2.conditions.zipWithIndex))
def acts(s: State) = s.editing.mapC(_.toList.flatMap(_._2.actions.zipWithIndex))
def addCond(s: State): Unit = upd(s)(r => r.copy(conditions = r.conditions :+ RuleConditionDto(RuleField.Subject, RuleOp.Contains, "", None)))
def addAct(s: State): Unit = upd(s)(r => r.copy(actions = r.actions :+ RuleActionDto(RuleAction.MarkRead, None)))
def dropCond(s: State, i: Int): Unit = upd(s)(r => r.copy(conditions = r.conditions.patch(i, Nil, 1)))
def dropAct(s: State, i: Int): Unit = upd(s)(r => r.copy(actions = r.actions.patch(i, Nil, 1)))

def saveRule(s: State): Unit = s.editing.v.foreach { (id, r) =>
  val f = id match {
    case Some(x) => Api.putUnit(s"/api/filters/$x", r.asJson)
    case None    => Api.post[FilterRuleDto]("/api/filters", r.asJson).map(_ => ())
  }
  Store.report(f) { _ => s.editing := None; loadRules(s); Store.toast("規則を保存しました") }
}
def deleteRule(s: State, r: FilterRuleDto): Unit = Dialog.confirm(s"規則「${r.name}」を削除しますか？", "削除") {
  Store.report(Api.delete(s"/api/filters/${r.id}"))(_ => loadRules(s))
}
def toggleRule(s: State, r: FilterRuleDto): Unit =
  Store.report(Api.putUnit(s"/api/filters/${r.id}", FilterRuleReq(r.name, !r.enabled, r.matchAll, r.conditions, r.actions).asJson))(_ => loadRules(s))
def moveRule(s: State, r: FilterRuleDto, dir: Int): Unit = {
  val l = s.rules.v
  val i = l.indexWhere(_.id == r.id)
  val j = i + dir
  if (j >= 0 && j < l.size) {
    val re = l.updated(i, l(j)).updated(j, l(i))
    s.rules := re
    Store.report(Api.putUnit("/api/filters/order", ReorderReq(re.map(_.id)).asJson))(_ => loadRules(s))
  }
}
def condText(c: RuleConditionDto): String =
  s"${if (c.field == RuleField.Header) c.header.getOrElse("") else RuleField.label(c.field)} が「${c.value}」${RuleOp.label(c.op)}"
def actText(a: RuleActionDto): String = RuleAction.label(a.kind) + a.arg.filter(_.nonEmpty).fold("")(x => s"（$x）")
def summary(r: FilterRuleDto): String = r.conditions.map(condText).mkString(if (r.matchAll) " かつ " else " または ") + " → " + r.actions.map(actText).mkString("、")
val fieldOpts: List[Opt] = RuleField.all.map(f => Opt(f, RuleField.label(f)))
val opOpts: List[Opt] = RuleOp.all.map(o => Opt(o, RuleOp.label(o)))
val actOpts: List[Opt] = RuleAction.all.map(a => Opt(a, RuleAction.label(a)))
def boxOpts = Store.mailboxes.mapC(_.map(b => Opt(b.name, Store.boxLabel(b))))

// ---------------------------------------------------------------- アドレス

def addresses = Store.me.mapC(_.toList.flatMap(_.addresses))
def setPrimary(a: AddressDto): Unit = Store.report(Api.putUnit(s"/api/me/primary-address/${a.id}", io.circe.Json.obj()))(_ => Store.reloadMe())

// ---------------------------------------------------------------- アプリパスワード・APIキー

def loadPasswords(s: State): Unit = Api.get[List[AppPasswordDto]]("/api/me/app-passwords").foreach(s.passwords := _)
def createPw(s: State): Unit =
  Store.report(Api.post[CreatedAppPasswordDto]("/api/me/app-passwords", CreateAppPasswordReq(s.pwName.v).asJson)) { r =>
    s.createdPw := Some(r); s.pwName := ""; loadPasswords(s)
  }
def deletePw(s: State, p: AppPasswordDto): Unit = Dialog.confirm(s"アプリパスワード「${p.name}」を削除しますか？そのメールソフトはログインできなくなります。", "削除") {
  Store.report(Api.delete(s"/api/me/app-passwords/${p.id}"))(_ => loadPasswords(s))
}
def pwText(s: State) = s.createdPw.mapC(_.map(_.password).getOrElse(""))
def pwUser(s: State) = s.createdPw.mapC(_.map(_.username).getOrElse(""))

def loadKeys(s: State): Unit = {
  Api.get[List[ApiKeyDto]]("/api/me/api-keys").foreach(s.keys := _)
  Api.get[List[OAuthGrantDto]]("/api/me/oauth-grants").foreach(s.grants := _)
}
def revokeGrant(s: State, g: OAuthGrantDto): Unit = Dialog.confirm(s"${g.name} との連携を取り消しますか？${g.name} は、あなたの権限で skereru を使えなくなります。", "取り消す") {
  Store.report(Api.delete(s"/api/me/oauth-grants/${js.URIUtils.encodeURIComponent(g.clientId)}"))(_ => loadKeys(s))
}
def grantScopes(g: OAuthGrantDto): String = g.scopes.map(sc => if (sc == ApiScope.Send) "メールの送信（send）" else s"${ApiScope.label(sc)}（$sc）").mkString("・")
def createKey(s: State): Unit =
  Store.report(Api.post[CreatedApiKeyDto]("/api/me/api-keys", CreateApiKeyReq(s.keyName.v, s.scopes.v, s.days.v.toIntOption).asJson)) { r =>
    s.createdKey := Some(r.key); s.keyName := ""; loadKeys(s)
  }
def revoke(s: State, k: ApiKeyDto): Unit = Dialog.confirm(s"APIキー「${k.name}」を失効させますか？使っているプログラム・AI は使えなくなります。", "失効") {
  Store.report(Api.delete(s"/api/me/api-keys/${k.id}"))(_ => loadKeys(s))
}
def origin: String = dom.window.location.origin
def mcpCommand(s: State) = s.createdKey.mapC(k => s"claude mcp add --transport http skereru $origin/mcp --header \"Authorization: Bearer ${k.getOrElse("<APIキー>")}\"")
def keyText(s: State) = s.createdKey.mapC(_.getOrElse(""))

// ---------------------------------------------------------------- メールの取り込み（Gmail など）

def unmounted(s: State): Unit = s.timer.v.foreach(dom.window.clearInterval)

def loadImports(s: State): Unit = Api.get[List[ImportJobDto]]("/api/imports").foreach(l => s.imports := l)
def active(j: ImportJobDto): Boolean = j.status == "queued" || j.status == "running"
def importStatus(j: ImportJobDto): String = j.status match {
  case "queued"    => "順番待ち"
  case "running"   => if (j.current.nonEmpty) s"取り込み中（${j.current}）" else "取り込み中"
  case "done"      => "完了"
  case "failed"    => "失敗"
  case "cancelled" => "取り消し"
  case other       => other
}
def importKind(j: ImportJobDto): String = if (j.kind == "imap") "IMAP" else "ファイル"
def counts(j: ImportJobDto): String = s"${j.imported} 件" + (if (j.skipped > 0) s"・重複 ${j.skipped}" else "") + (if (j.failed > 0) s"・失敗 ${j.failed}" else "")

def startImap(s: State): Unit =
  if (s.imHost.v.trim.isEmpty || s.imUser.v.trim.isEmpty || s.imPass.v.isEmpty) Dialog.alert("サーバー・メールアドレス・パスワードを入力してください")
  else {
    s.imBusy := true
    Api.post[ImportJobDto]("/api/imports/imap", ImapImportReq(s.imHost.v.trim, s.imPort.v.toIntOption.getOrElse(993), s.imUser.v.trim, s.imPass.v).asJson).onComplete { r =>
      s.imBusy := false
      r match {
        case scala.util.Success(_) =>
          s.imPass := ""
          Store.toast("取り込みを始めました。この画面を閉じても、サーバーが続けます")
          loadImports(s)
        case scala.util.Failure(e) => Store.error(e)
      }
    }
  }

def cancelImport(s: State, j: ImportJobDto): Unit =
  if (j.kind == "file" && s.fileRunning.v) s.fileCancel := true
  else Dialog.confirm(s"「${j.source}」の取り込みを取り消しますか？（取り込んだ分はそのまま残ります）", "取り消す") {
    Api.post[UpdatedDto](s"/api/imports/${j.id}/cancel").foreach(_ => loadImports(s))
  }

def pickFiles(s: State, e: dom.Event): Unit = {
  val input = e.target.asInstanceOf[dom.HTMLInputElement]
  val files = input.files
  runFiles(s, files)
  input.value = ""
}

def dropFiles(s: State, e: dom.DragEvent): Unit = { s.dropping := false; runFiles(s, e.dataTransfer.files) }

def runFiles(s: State, files: dom.FileList): Unit =
  if (files.length > 0 && !s.fileRunning.v) {
    s.fileRunning := true; s.fileCancel := false; s.fileProgress := "準備中…"
    def pct(p: FileImport.Progress) = if (p.total <= 0) 100 else math.min(100, (p.read * 100 / p.total).toInt)
    FileImport.run(files, p => {
      s.fileProgress := s"${p.file} を読み込み中 ${pct(p)}% ・取り込み ${p.imported} 件" +
        (if (p.skipped > 0) s"・重複 ${p.skipped} 件" else "") + (if (p.failed > 0) s"・失敗 ${p.failed} 件" else "")
      if (p.imported > 0) Store.changed()
    }, () => s.fileCancel.v).onComplete { r =>
      s.fileRunning := false; s.fileProgress := ""
      r match {
        case scala.util.Success(p) =>
          Store.toast(if (s.fileCancel.v) s"取り込みを中止しました（${p.imported} 件は取り込み済み）" else s"${p.imported} 件を取り込みました")
        case scala.util.Failure(e) => Store.error(e)
      }
      Store.changed(); loadImports(s)
    }
    loadImports(s)
  }

/** 書き出すメールボックスの選択肢（空はすべて）. */
def exportOptions = Store.mailboxes.mapC(bs => Opt.of(("" -> "すべてのメールボックス") :: bs.map(b => b.id.toString -> Store.boxLabel(b))))
def exportUrl(s: State) = s.exportBox.mapC(b => "/api/export" + (if (b.isEmpty) "" else "?mailboxId=" + b))

def dropClass(s: State) = s.dropping.mapC(d => if (d) "dropzone on" else "dropzone")

// ---------------------------------------------------------------- 連携（skuaka）

def skUrl(s: State) = s.integ.mapC(_.flatMap(_.skuakaUrl).getOrElse(""))
def skAvailable(s: State) = s.integ.mapC(_.exists(_.skuakaUrl.isDefined))
def skConnected(s: State) = s.integ.mapC(_.exists(_.skuakaConnected))
def connectSkuaka(s: State): Unit =
  Store.report(Api.put[IntegrationsDto]("/api/me/integrations/skuaka", TokenReq(s.skToken.v.trim).asJson)) { i =>
    s.integ := Some(i); s.skToken := ""; Store.toast("skuaka とつなぎました")
  }
def disconnectSkuaka(s: State): Unit =
  Dialog.confirm("skuaka との連携を解除しますか？（預かっているトークンを消します）", "解除") {
    Store.report(Api.send(dom.HttpMethod.DELETE, "/api/me/integrations/skuaka")) { _ =>
      Api.get[IntegrationsDto]("/api/me/integrations").foreach(i => s.integ := Some(i))
    }
  }

def maxMb = cfg.mapC(_.map(_.maxMessageMb.toString).getOrElse("50"))
def cfg = Store.config
def host = cfg.mapC(_.map(_.hostname).getOrElse(""))
def port(f: ClientConfigDto => Option[Int]) = cfg.mapC(_.flatMap(f).map(_.toString).getOrElse("（使えません）"))
</script>

<template>
  <div class="page narrow">
    <h1>設定</h1>
    <nav class="tabs">
      <button v-for="t in tabs" :class="tabClass(s, t._1)" @click="s.tab := t._1">{{ t._2 }}</button>
    </nav>

    <template v-if="on(s, &quot;general&quot;)">
      <section class="panel">
        <h2>アイコン</h2>
        <div class="row avatar-row">
          <Avatar :name="myName.v" :picture="myPicture.v" :size="64" />
          <label class="btn">画像を選ぶ<input type="file" accept="image/png,image/jpeg,image/gif,image/webp" class="hidden-file" @change="pickAvatar(event)"></label>
          <button v-if="uploaded" type="button" class="ghost" @click="removeAvatar()">消す</button>
        </div>
        <p class="muted small">中央を正方形に切り抜いて保存します。保存しないときは、ログインの仕組み（sruru など）に登録したアイコンを使います。</p>
      </section>
      <section class="panel">
        <h2>名前</h2>
        <div class="row"><input type="text" v-model="s.name" class="grow"><button class="primary" @click="saveName(s)">保存</button></div>
        <p class="muted small">送るメールの差出人の名前になります。</p>
      </section>
      <section class="panel">
        <h2>表示</h2>
        <label class="row"><input type="checkbox" v-model="s.conversation"> スレッド表示（同じ話題のメールをまとめる）</label>
        <label class="row"><input type="checkbox" v-model="s.images"> 外部の画像を常に表示する（送信者に開封が知られることがあります）</label>
        <div class="field"><span>1ページの件数</span><Combo :bind="Some(s.pageSize)" :options='Opt.names(List("25", "50", "100", "200"))' /></div>
        <div class="field" title="送信を押してから、この時間のあいだは送信を取り消せます"><span>送信の取り消し</span>
          <Combo :bind="Some(s.undoSecs)" :options='Opt.of(List("0" -> "使わない（すぐ送る）", "5" -> "5 秒", "10" -> "10 秒", "20" -> "20 秒", "30" -> "30 秒"))' /></div>
        <div class="field" title="この端末のブラウザで、画面を見ていないときに新しいメールを知らせます"><span>新着の通知（この端末）</span>
          <button v-if="Notify.state.mapC(_ != &quot;on&quot;)" class="small" @click="Notify.enable()">有効にする（ブラウザが許可を求めます）</button>
          <button v-if="Notify.state.mapC(_ == &quot;on&quot;)" class="small" @click="Notify.disable()">無効にする</button></div>
        <div class="field" title="受信トレイに新しいメールが届いたとき、画面を閉じていてもこの端末に知らせます"><span>プッシュ通知（この端末）</span>
          <PushToggle /></div>
        <p class="muted small">キーボードのショートカットは <kbd>?</kbd> で一覧を出せます。</p>
        <div class="field"><span>画面の色（この端末に記憶します）</span>
          <div class="chips"><button v-for="o in Theme.options" :class='Theme.current.mapC(c => if (c == o._1) "chip active" else "chip")' @click="Theme.set(o._1)">{{ o._2 }}</button></div>
        </div>
      </section>
      <section class="panel">
        <h2>署名</h2>
        <textarea v-model="s.signature" rows="5" placeholder="山田 太郎&#10;株式会社〇〇"></textarea>
      </section>
      <button class="primary" @click="save(s)">保存</button>
    </template>

    <template v-if="on(s, &quot;vacation&quot;)">
      <section class="panel">
        <h2>自動返信（不在のお知らせ）</h2>
        <label class="row"><input type="checkbox" v-model="s.vacOn"> 自動返信を使う</label>
        <div class="row">
          <label class="field"><span>開始日</span><input type="date" v-model="s.vacStart"></label>
          <label class="field"><span>終了日</span><input type="date" v-model="s.vacEnd"></label>
        </div>
        <label class="field"><span>件名</span><input type="text" v-model="s.vacSubject"></label>
        <label class="field"><span>本文</span><textarea v-model="s.vacBody" rows="6"></textarea></label>
        <label class="row"><input type="checkbox" v-model="s.vacContacts"> やりとりしたことのある相手にだけ返信する</label>
        <p class="muted small">同じ相手には4日に1回だけ返信します。メーリングリスト・自動送信のメールには返信しません。</p>
      </section>
      <button class="primary" @click="save(s)">保存</button>
    </template>

    <template v-if="on(s, &quot;filters&quot;)">
      <section class="panel">
        <div class="row"><h2 class="grow">振り分けの規則</h2><button class="primary small" @click="newRule(s)">＋ 規則を追加</button></div>
        <p class="muted small">受け取ったメールに上から順に当てはめます（迷惑メールの判定の後）。</p>
        <div v-for="r in s.rules" :key="r.id" class="rule">
          <input type="checkbox" :checked="r.enabled" @change="toggleRule(s, r)" title="有効">
          <div class="grow"><b>{{ r.name }}</b><div class="muted small">{{ summary(r) }}</div></div>
          <button class="ghost small" title="上へ（先に適用します）" @click="moveRule(s, r, -1)">↑</button>
          <button class="ghost small" title="下へ" @click="moveRule(s, r, 1)">↓</button>
          <button class="small" @click="editRule(s, r)">編集</button>
          <button class="danger small" @click="deleteRule(s, r)">削除</button>
        </div>
        <p v-if="s.rules.mapC(_.isEmpty)" class="muted">規則はありません</p>
      </section>
      <Modal v-for="f in form(s)" :onClose="_ => s.editing := None" :wide="true">
        <h2>振り分けの規則</h2>
        <label class="field"><span>名前</span><input type="text" :value="f.name" @input="upd(s)(_.copy(name = Dom.value(event)))"></label>
        <div class="row small">
          <label><input type="radio" name="m" :checked="f.matchAll" @change="upd(s)(_.copy(matchAll = true))"> すべての条件に一致</label>
          <label><input type="radio" name="m" :checked="!f.matchAll" @change="upd(s)(_.copy(matchAll = false))"> いずれかの条件に一致</label>
        </div>
        <h3>条件</h3>
        <div v-for="ci in conds(s)" class="row cond">
          <Combo :value="ci._1.field" :options="fieldOpts" :small="true" :onChange="v => setCond(s, ci._2)(_.copy(field = v))" />
          <input v-if="ci._1.field == RuleField.Header" type="text" placeholder="ヘッダー名" :value="ci._1.header.getOrElse(&quot;&quot;)" @input="setCond(s, ci._2)(_.copy(header = Some(Dom.value(event))))" class="hname">
          <Combo :value="ci._1.op" :options="opOpts" :small="true" :onChange="v => setCond(s, ci._2)(_.copy(op = v))" />
          <input type="text" :value="ci._1.value" @input="setCond(s, ci._2)(_.copy(value = Dom.value(event)))" class="grow">
          <button class="ghost small" title="この条件を削除" @click="dropCond(s, ci._2)">✕</button>
        </div>
        <button class="small" @click="addCond(s)">＋ 条件</button>
        <h3>処理</h3>
        <div v-for="ai in acts(s)" class="row cond">
          <Combo :value="ai._1.kind" :options="actOpts" :small="true" :onChange="v => setAct(s, ai._2)(_.copy(kind = v, arg = None))" />
          <Combo v-if="ai._1.kind == RuleAction.Move" :value="ai._1.arg.getOrElse(&quot;&quot;)" :live="Some(boxOpts)" :small="true" :onChange="v => setAct(s, ai._2)(_.copy(arg = Some(v)))" />
          <input v-if="ai._1.kind == RuleAction.Forward" type="email" placeholder="転送先のアドレス" :value="ai._1.arg.getOrElse(&quot;&quot;)" @input="setAct(s, ai._2)(_.copy(arg = Some(Dom.value(event))))" class="grow">
          <span class="grow"></span>
          <button class="ghost small" title="この動作を削除" @click="dropAct(s, ai._2)">✕</button>
        </div>
        <button class="small" @click="addAct(s)">＋ 処理</button>
        <div class="actions">
          <button @click="s.editing := None">キャンセル</button>
          <button class="primary" @click="saveRule(s)">保存</button>
        </div>
      </Modal>
    </template>

    <template v-if="on(s, &quot;spam&quot;)">
      <section class="panel">
        <h2>迷惑メール</h2>
        <p class="muted small">受け取ったメールは、送信ドメイン認証（SPF・DKIM・DMARC）・送信元の評判・内容・学習した傾向で判定し、迷惑メールのフォルダに振り分けます。
          一覧の「🚫」で報告すると、あなたの判定の傾向を学習します。</p>
        <label class="field"><span>常に受け取る差出人（1行に1つ。アドレス・@ドメイン）</span><textarea v-model="s.allow" rows="5" placeholder="boss@example.com&#10;@partner.example"></textarea></label>
        <label class="field"><span>常に迷惑メールにする差出人</span><textarea v-model="s.block" rows="5"></textarea></label>
      </section>
      <button class="primary" @click="save(s)">保存</button>
    </template>

    <template v-if="on(s, &quot;addresses&quot;)">
      <section class="panel">
        <h2>自分のアドレス</h2>
        <table class="list">
          <thead><tr><th>アドレス</th><th>送信</th><th></th></tr></thead>
          <tbody>
            <tr v-for="a in addresses" :key="a.id">
              <td>{{ a.address }} <span v-if="a.isPrimary" class="badge">主アドレス</span></td>
              <td>{{ if (a.canSend) "差出人に使える" else "受信のみ" }}</td>
              <td><button v-if="!a.isPrimary" class="small" @click="setPrimary(a)">主アドレスにする</button></td>
            </tr>
          </tbody>
        </table>
        <p class="muted small">アドレスの追加・別名は管理者に依頼してください。</p>
      </section>
    </template>

    <template v-if="on(s, &quot;clients&quot;)">
      <section class="panel">
        <h2>メールソフトの設定</h2>
        <p class="muted small">Thunderbird・Outlook・iPhone のメールなどで使えます。パスワードには下の「アプリパスワード」を使います（ログインのパスワードは使えません）。</p>
        <table class="list">
          <tbody>
            <tr><th>サーバー</th><td><code>{{ host }}</code></td></tr>
            <tr><th>ユーザー名</th><td>メールアドレス</td></tr>
            <tr><th>受信（IMAP）</th><td>ポート {{ port(_.imapsPort) }}（SSL/TLS）または {{ port(_.imapPort) }}（STARTTLS）</td></tr>
            <tr><th>受信（POP3）</th><td>ポート {{ port(_.pop3sPort) }}（SSL/TLS）または {{ port(_.pop3Port) }}（STARTTLS）</td></tr>
            <tr><th>送信（SMTP）</th><td>ポート {{ port(_.smtpsPort) }}（SSL/TLS）または {{ port(_.submissionPort) }}（STARTTLS）・認証あり</td></tr>
          </tbody>
        </table>
      </section>
      <section class="panel">
        <h2>アプリパスワード</h2>
        <form class="row" @submit.prevent="createPw(s)">
          <input type="text" v-model="s.pwName" placeholder="用途（例: 会社の PC の Thunderbird）" class="grow">
          <button type="submit" class="primary">発行</button>
        </form>
        <div v-if="s.createdPw.mapC(_.isDefined)" class="created">
          <p><b>アプリパスワードを発行しました。</b>この画面を離れると二度と表示できません。</p>
          <p>ユーザー名: <code>{{ pwUser(s) }}</code></p>
          <div class="row"><code class="grow keytext big">{{ pwText(s) }}</code><button class="small" @click="Dom.copy(pwText(s).v)">コピー</button></div>
          <button class="small" @click="s.createdPw := None">閉じる</button>
        </div>
        <table class="list">
          <thead><tr><th>用途</th><th>先頭</th><th>作成</th><th>最終使用</th><th></th></tr></thead>
          <tbody>
            <tr v-for="p in s.passwords" :key="p.id">
              <td>{{ p.name }}</td><td><code>{{ p.prefix }}…</code></td><td class="small">{{ Fmt.dateTime(p.createdAt) }}</td>
              <td class="small">{{ p.lastUsedAt.map(Fmt.ago).getOrElse("未使用") }} {{ p.lastUsedIp.getOrElse("") }}</td>
              <td><button class="danger small" @click="deletePw(s, p)">削除</button></td>
            </tr>
            <tr v-if="s.passwords.mapC(_.isEmpty)"><td colspan="5" class="muted">アプリパスワードはありません</td></tr>
          </tbody>
        </table>
        <p class="muted small">認証に続けて失敗すると、その IP アドレスとアカウントは15分間ロックされます。</p>
      </section>
    </template>

    <template v-if="on(s, &quot;import&quot;)">
      <section class="panel">
        <h2>Gmail などから取り込む（IMAP）</h2>
        <p class="muted small">ほかのメールサービスのフォルダとメールを、まるごとこのサーバーに取り込みます。取り込み元のメールは変わりません（既読にもなりません）。
          取り込みはサーバーが裏で進めるので、この画面を閉じてもかまいません。入力したパスワードは、取り込みが終わると消します。</p>
        <p class="small"><b>Gmail の場合:</b> Google アカウントで2段階認証を有効にしてから
          <a href="https://myaccount.google.com/apppasswords" target="_blank" rel="noopener noreferrer">アプリ パスワード</a> を作り、下のパスワードに入力します（ふだんのパスワードでは入れません）。
          ラベルはフォルダとして取り込み、どのラベルもないメールは「アーカイブ」に入れます。</p>
        <form @submit.prevent="startImap(s)">
          <div class="row">
            <label class="grow">サーバー<input type="text" v-model="s.imHost" placeholder="imap.gmail.com" autocomplete="off"></label>
            <label>ポート<Combo :bind="Some(s.imPort)" :options='Opt.of(List("993" -> "993（SSL/TLS）", "143" -> "143（STARTTLS）"))' /></label>
          </div>
          <div class="row">
            <label class="grow">メールアドレス（ユーザー名）<input type="email" v-model="s.imUser" placeholder="you@gmail.com" autocomplete="off"></label>
            <label class="grow">パスワード（アプリ パスワード）<input type="password" v-model="s.imPass" autocomplete="new-password"></label>
          </div>
          <button type="submit" class="primary" :disabled="s.imBusy">{{ if (s.imBusy.v) "ログインを確かめています…" else "取り込みを始める" }}</button>
        </form>
      </section>

      <section class="panel">
        <h2>ファイルから取り込む（mbox・.eml）</h2>
        <p class="muted small"><a href="https://takeout.google.com/" target="_blank" rel="noopener noreferrer">Google Takeout</a> で書き出した「メール」（.mbox）なら、
          ラベル・スター・未読もそのまま引き継ぎます。Thunderbird などの mbox はファイル名のフォルダに、.eml は「取り込み」フォルダに入れます。
          同じメールは二重に取り込みません。取り込みが終わるまで、この画面を開いたままにしてください。</p>
        <label :class="dropClass(s)" title="ファイルを選ぶか、ここにドロップします"
          @dragover.prevent="s.dropping := true" @dragleave="s.dropping := false" @drop.prevent="dropFiles(s, event.asInstanceOf[dom.DragEvent])">
          <input type="file" multiple accept=".mbox,.mbx,.eml,message/rfc822,application/mbox" class="hidden" :disabled="s.fileRunning" @change="pickFiles(s, event)">
          <span>📥 ファイルを選ぶ（またはここにドロップ）</span>
        </label>
        <div v-if="s.fileRunning" class="row progress">
          <span class="grow small">{{ s.fileProgress }}</span>
          <button class="small" @click="s.fileCancel := true">中止</button>
        </div>
      </section>

      <section class="panel">
        <h2>書き出す（mbox）</h2>
        <p class="muted small">メールを mbox のファイルに書き出します。フォルダ・スター・未読は Google Takeout と同じ形で書くので、
          このサーバーやほかのメールサービスに、上の「ファイルから取り込む」で戻せます（同じアカウントに戻したときは、重複として飛ばします）。</p>
        <div class="row">
          <Combo :bind="Some(s.exportBox)" :live="Some(exportOptions)" />
          <a class="btn primary" :href="exportUrl(s)" download title="選んだメールボックスを mbox で書き出す">書き出す</a>
        </div>
      </section>

      <section class="panel">
        <h2>取り込みの履歴</h2>
        <table class="list">
          <thead><tr><th>取り込み元</th><th>状態</th><th>件数</th><th>開始</th><th></th></tr></thead>
          <tbody>
            <tr v-for="j in s.imports" :key="j.id">
              <td><span class="muted small">{{ importKind(j) }}</span> {{ j.source }}<div v-if="j.error.isDefined" class="small err">{{ j.error.getOrElse("") }}</div></td>
              <td class="small">{{ importStatus(j) }}</td>
              <td class="small">{{ counts(j) }}</td>
              <td class="small">{{ Fmt.dateTime(j.createdAt) }}</td>
              <td><button v-if="active(j)" class="small" @click="cancelImport(s, j)">取り消し</button></td>
            </tr>
            <tr v-if="s.imports.mapC(_.isEmpty)"><td colspan="5" class="muted">取り込みはまだありません</td></tr>
          </tbody>
        </table>
        <p class="muted small">失敗は、大きすぎる（{{ maxMb }}MB を超える）・容量の上限を超えたメールです。</p>
      </section>
    </template>

    <template v-if="on(s, &quot;integrations&quot;)">
      <section class="panel">
        <h2>skuaka（カレンダー）</h2>
        <p class="muted small">会議の招待のメールにある「カレンダーに追加」で、予定を skuaka のカレンダーに足せるようにします。</p>
        <p v-if="skAvailable(s).mapC(!_)" class="small">このサーバーでは skuaka と連携できません（管理者が <code>SKUAKA_URL</code> を設定すると使えます）。</p>
        <template v-if="skAvailable(s)">
          <p class="small">skuaka: <a :href="skUrl(s)" target="_blank" rel="noopener noreferrer">{{ skUrl(s) }}</a>
            ・状態: <b>{{ if (skConnected(s).v) "つながっています" else "つながっていません" }}</b></p>
          <form v-if="skConnected(s).mapC(!_)" class="row" @submit.prevent="connectSkuaka(s)">
            <input type="password" v-model="s.skToken" placeholder="skuaka の API トークン（skuaka_…）" class="grow" autocomplete="off">
            <button type="submit" class="primary">つなぐ</button>
          </form>
          <p v-if="skConnected(s).mapC(!_)" class="muted small">skuaka の「設定 → API トークンと MCP」で、読み書きのトークンを作って貼り付けてください。トークンは暗号化して預かります。</p>
          <button v-if="skConnected(s)" class="danger small" @click="disconnectSkuaka(s)">連携を解除</button>
        </template>
      </section>
    </template>

    <template v-if="on(s, &quot;api&quot;)">
      <section class="panel">
        <h2>APIキー（REST API・MCP）</h2>
        <p class="muted small">プログラムや AI（MCP）から使うためのキーです。<code>Authorization: Bearer &lt;キー&gt;</code> で送ります。メールの送信には send のスコープが必要です。</p>
        <form @submit.prevent="createKey(s)">
          <div class="row">
            <input type="text" v-model="s.keyName" placeholder="用途（例: Claude Code）" class="grow">
            <Combo :bind="Some(s.days)" :options='Opt.of(List("30" -> "30日", "90" -> "90日", "365" -> "1年", "" -> "無期限"))' />
          </div>
          <div class="row scopes">
            <span class="small muted">スコープ:</span>
            <label v-for="sc in ApiScope.all.filterNot(_ == ApiScope.Admin)" class="small"><input type="checkbox" :value="sc" v-model="s.scopes"> {{ ApiScope.label(sc) }}（{{ sc }}）</label>
            <label v-if="Store.isAdmin" class="small"><input type="checkbox" value="admin" v-model="s.scopes"> 管理（admin）</label>
          </div>
          <button type="submit" class="primary">発行</button>
        </form>
        <div v-if="s.createdKey.mapC(_.isDefined)" class="created">
          <p><b>APIキーを発行しました。</b>この画面を離れると二度と表示できません。</p>
          <div class="row"><code class="grow keytext">{{ keyText(s) }}</code><button class="small" @click="Dom.copy(keyText(s).v)">コピー</button></div>
          <p class="small">Claude Code から使う（MCP）:</p>
          <div class="row"><code class="grow keytext">{{ mcpCommand(s) }}</code><button class="small" @click="Dom.copy(mcpCommand(s).v)">コピー</button></div>
          <button class="small" @click="s.createdKey := None">閉じる</button>
        </div>
        <table class="list">
          <thead><tr><th>用途</th><th>キー</th><th>スコープ</th><th>有効期限</th><th>最終使用</th><th></th></tr></thead>
          <tbody>
            <tr v-for="k in s.keys" :key="k.id">
              <td>{{ k.name }}</td><td><code>{{ k.prefix }}…</code></td><td>{{ k.scopes.mkString(", ") }}</td>
              <td class="small">{{ k.expiresAt.map(Fmt.dateTime).getOrElse("無期限") }}</td>
              <td class="small">{{ k.lastUsedAt.map(Fmt.ago).getOrElse("未使用") }}</td>
              <td><button class="danger small" @click="revoke(s, k)">失効</button></td>
            </tr>
            <tr v-if="s.keys.mapC(_.isEmpty)"><td colspan="6" class="muted">APIキーはありません</td></tr>
          </tbody>
        </table>
        <p class="small muted">MCP: <code>{{ origin }}/mcp</code>（Streamable HTTP）。REST API の一覧: <a href="/api/endpoints" target="_blank">/api/endpoints</a></p>
      </section>
      <section class="panel">
        <h2>連携しているサービス</h2>
        <p class="muted small">あなたが許可して、あなたの権限で skereru を使っているサービス（skakapo など）です。取り消すと、そのサービスはすぐに使えなくなります。</p>
        <table class="list">
          <thead><tr><th>サービス</th><th>できること</th><th>許可した日時</th><th>最終使用</th><th></th></tr></thead>
          <tbody>
            <tr v-for="g in s.grants" :key="g.clientId">
              <td>{{ g.name }}</td><td class="small">{{ grantScopes(g) }}</td>
              <td class="small">{{ Fmt.dateTime(g.createdAt) }}</td>
              <td class="small">{{ g.lastUsedAt.map(Fmt.ago).getOrElse("未使用") }}</td>
              <td><button class="danger small" @click="revokeGrant(s, g)">取り消す</button></td>
            </tr>
            <tr v-if="s.grants.mapC(_.isEmpty)"><td colspan="5" class="muted">連携しているサービスはありません</td></tr>
          </tbody>
        </table>
      </section>
    </template>
  </div>
</template>

<style scoped>
.avatar-row { align-items: center; gap: .8rem; }
.hidden-file { display: none; }
.tabs { flex-wrap: nowrap; }
.rule { display: flex; align-items: center; gap: .5rem; padding: .6rem 0; border-bottom: 1px solid var(--line); }
.cond { margin-bottom: .4rem; }
.hname { width: 9rem; }
.scopes { margin: .5rem 0; }
.created { border: 1px solid var(--ok); border-radius: 8px; padding: .8rem 1rem; margin: 1rem 0; background: color-mix(in srgb, var(--ok) 8%, transparent); }
.keytext { background: var(--side); padding: .3rem .5rem; border-radius: 5px; word-break: break-all; font-size: .8rem; }
.keytext.big { font-size: 1.2rem; letter-spacing: .08em; }
table.list th { width: auto; }
textarea { font-family: inherit; }
.dropzone { display: flex; align-items: center; justify-content: center; padding: 1.4rem; border: 2px dashed var(--line); border-radius: 10px; cursor: pointer; color: var(--muted); }
.dropzone:hover, .dropzone.on { border-color: var(--accent); color: var(--accent); background: var(--accent-weak); }
.progress { margin-top: .6rem; }
.err { color: var(--danger); }
label.grow { display: flex; flex-direction: column; gap: .2rem; font-size: .85rem; }
</style>
