package skereru.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skereru.shared.*

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** 書きかけのメール（作成画面）. */
final case class Draft(seq: Int, init: ComposeReq, minimized: Boolean = false, uploads: List[UploadDto] = Nil)

/** 画面全体で共有する状態と、変更の自動同期（SSE で知らせを受けて取りに行く. つながらないときは30秒ごと）. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val settings: RV[MailSettingsDto] = RV(MailSettingsDto.default)
  val mailboxes: RV[List[MailboxDto]] = RV(Nil)

  /** 変更があるたびに増える. 一覧・スレッドの画面はこれを監視して取り直す. */
  val tick: RV[Int] = RV(0)

  /** 開いている作成画面. */
  val drafts: RV[List[Draft]] = RV(Nil)

  /** 左のメニュー（スマートフォンでは開閉する）. */
  val nav: RV[Boolean] = RV(false)

  private var draftSeq = 0

  def isAdmin = me.mapC(_.exists(_.isAdmin))

  /** 送信に使えるアドレス（主アドレスを先に）. */
  def sendable: List[String] =
    me.v.toList.flatMap(_.addresses.filter(_.canSend).sortBy(a => (!a.isPrimary, a.address)).map(_.address))

  def special(use: String): Option[MailboxDto] = mailboxes.v.find(_.specialUse.contains(use))

  /** URL の名前（inbox・sent… か メールボックスの ID）からメールボックスを探す. */
  def boxByKey(key: String): Option[MailboxDto] =
    if (SpecialUse.all.contains(key)) special(key) else mailboxes.v.find(_.id.toString == key)

  def boxKey(b: MailboxDto): String = b.specialUse.getOrElse(b.id.toString)

  /**
   * メールボックスではない一覧（左メニューのスター付き・スヌーズ中）。すべてのメールボックス（ゴミ箱・迷惑メールを除く）を、条件で探す.
   * path: URL、query: 検索の条件、title: 見出し、icon、tip: ヒント. 予約済み（/scheduled）は送信前のものなので、別の画面.
   */
  final case class VirtualList(path: String, query: String, title: String, icon: String, tip: String)
  val virtualLists: List[VirtualList] = List(
    VirtualList("/starred", "is:starred", "スター付き", "⭐", "スターを付けたメール（ゴミ箱・迷惑メールを除く）"),
    VirtualList("/snoozed", "is:snoozed", "スヌーズ中", "⏰", "スヌーズしたメール（指定した日時に受信トレイへ戻ります）")
  )
  def virtualList(path: String): Option[VirtualList] = virtualLists.find(_.path == path)
  val ScheduledPath = "/scheduled"

  def boxLabel(b: MailboxDto): String = b.specialUse.map(SpecialUse.label).getOrElse(b.name)

  // ---------------------------------------------------------------- 読み込み

  def loadMailboxes(): Future[List[MailboxDto]] = Api.mailboxes().map { b =>
    mailboxes := b
    b.find(_.specialUse.contains(SpecialUse.Inbox)).foreach(i => Notify.inboxUnread(i.unread))
    b
  }

  def loadSettings(): Unit = Api.get[MailSettingsDto]("/api/me/settings").foreach(settings := _)

  def reloadMe(): Unit = Api.me().foreach(m => me := Some(m))

  /** skawau（ファイル共有）との連携の状態（作成の画面・スレッドの画面で「skawau から」「skawau に保存」を出すか）. */
  val skawau: RV[Option[LinkStatusDto]] = RV(None)
  def loadSkawau(): Unit = Api.get[LinkStatusDto]("/api/integrations/skawau").foreach(l => skawau := Some(l))
  def skawauReady = skawau.mapC(_.exists(l => l.configured && l.connected))

  /** 何かを変えた直後（一覧・件数を取り直す）. */
  def changed(): Unit = { loadMailboxes(); tick := tick.v + 1 }

  // ---------------------------------------------------------------- 作成画面

  def compose(init: ComposeReq, uploads: List[UploadDto] = Nil): Unit = {
    draftSeq += 1
    drafts := (drafts.v.map(_.copy(minimized = true)) :+ Draft(draftSeq, init, uploads = uploads)).takeRight(3)
  }

  /** スヌーズする（日時を選んでもらう）. 終わったら after. */
  def snooze(ids: List[java.util.UUID])(after: () => Unit): Unit =
    if (ids.nonEmpty) When.pick("スヌーズ（この日時まで一覧から隠し、受信トレイに戻します）") { t =>
      Toasts.report(Api.post[UpdatedDto](
        "/api/messages/snooze",
        io.circe.syntax.EncoderOps(SnoozeReq(ids, Some(t))).asJson
      )) {
        _ =>
          Toasts.withAction(s"${When.label(t.toDate)} までスヌーズしました", "元に戻す", Some(8000))(unsnooze(ids))
          changed(); after()
      }
    }

  def unsnooze(ids: List[java.util.UUID]): Unit =
    Toasts.report(Api.post[UpdatedDto](
      "/api/messages/snooze",
      io.circe.syntax.EncoderOps(SnoozeReq(ids, None)).asJson
    )) { _ =>
      Toasts.show("スヌーズを解除しました"); changed()
    }

  /** 予約した送信を取り消して、作成画面に戻す. */
  def unschedule(id: java.util.UUID): Unit =
    Toasts.report(Api.post[ScheduledCancelDto](s"/api/scheduled/$id/cancel")) { c =>
      compose(c.req, c.uploads); Toasts.show("送信を取り消しました"); changed()
    }

  def newMail(to: List[String] = Nil): Unit = {
    val sig = settings.v.signature.trim
    compose(ComposeReq(
      sendable.headOption.getOrElse(""),
      to,
      Nil,
      Nil,
      "",
      if (sig.isEmpty) "" else s"\n\n-- \n$sig",
      None,
      Nil,
      None,
      None,
      None
    ))
  }

  def closeDraft(seq: Int): Unit = drafts := drafts.v.filterNot(_.seq == seq)

  def toggleDraft(seq: Int): Unit = drafts :=
    drafts.v.map(d => if (d.seq == seq) d.copy(minimized = !d.minimized) else d)

  // ---------------------------------------------------------------- 自動同期（SSE）

  private var stream: Option[dom.EventSource] = None
  private var pending: Option[Int] = None
  private var lastSync = 0.0

  private def soon(): Unit =
    if (pending.isEmpty) pending = Some(dom.window.setTimeout(() => { pending = None; sync() }, 300))

  private def sync(): Unit = { lastSync = js.Date.now(); changed() }

  private def open(): Unit = {
    val es = new dom.EventSource("/api/stream")
    es.addEventListener("change", (_: dom.MessageEvent) => soon())
    stream = Some(es)
  }

  def connect(): Unit = if (me.v.isDefined && stream.isEmpty) {
    open()
    dom.window.setInterval(
      () => {
        // サーバーに断られた（同時接続の上限など）ときは、ブラウザは自動ではつなぎ直さないので、ここでつなぎ直す
        if (stream.exists(_.readyState == dom.EventSource.CLOSED)) open()
        val live = stream.exists(_.readyState == dom.EventSource.OPEN)
        if (dom.document.visibilityState.toString == "visible" && (!live && js.Date.now() - lastSync > 30000)) sync()
      },
      10000
    )
    dom.document.addEventListener(
      "visibilitychange",
      (_: dom.Event) => if (dom.document.visibilityState.toString == "visible") soon()
    )
  }

}

/** この端末だけの表示の設定（localStorage. 使えないときは既定値）. */
object Prefs extends sbirds.web.LocalPrefs("skereru")

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme
  extends sbirds.web.ThemeSwitch(Prefs, List("system" -> "🖥 自動", "light" -> "☀️ ライト", "dark" -> "🌙 ダーク"))
