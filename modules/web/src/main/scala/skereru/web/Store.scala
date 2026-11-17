package skereru.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skereru.shared.*

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}

final case class Toast(id: Int, text: String, error: Boolean, undo: Option[() => Unit], undoLabel: String = "元に戻す")

/** 書きかけのメール（作成画面）. */
final case class Draft(seq: Int, init: ComposeReq, minimized: Boolean = false, uploads: List[UploadDto] = Nil)

/** 画面全体で共有する状態と、変更の自動同期（SSE で知らせを受けて取りに行く. つながらないときは30秒ごと）. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val settings: RV[MailSettingsDto] = RV(MailSettingsDto.default)
  val mailboxes: RV[List[MailboxDto]] = RV(Nil)
  val toasts: RV[List[Toast]] = RV(Nil)
  /** 変更があるたびに増える. 一覧・スレッドの画面はこれを監視して取り直す. */
  val tick: RV[Int] = RV(0)
  /** 開いている作成画面. */
  val drafts: RV[List[Draft]] = RV(Nil)
  /** 左のメニュー（スマートフォンでは開閉する）. */
  val nav: RV[Boolean] = RV(false)

  private var toastSeq = 0
  private var draftSeq = 0

  def isAdmin = me.mapC(_.exists(_.isAdmin))

  /** 送信に使えるアドレス（主アドレスを先に）. */
  def sendable: List[String] = me.v.toList.flatMap(_.addresses.filter(_.canSend).sortBy(a => (!a.isPrimary, a.address)).map(_.address))

  def special(use: String): Option[MailboxDto] = mailboxes.v.find(_.specialUse.contains(use))

  /** URL の名前（inbox・sent… か メールボックスの ID）からメールボックスを探す. */
  def boxByKey(key: String): Option[MailboxDto] =
    if (SpecialUse.all.contains(key)) special(key) else mailboxes.v.find(_.id.toString == key)

  def boxKey(b: MailboxDto): String = b.specialUse.getOrElse(b.id.toString)

  /** メールボックスではない一覧（左メニューのスター付き・スヌーズ中）。すべてのメールボックス（ゴミ箱・迷惑メールを除く）を、条件で探す.
    * path: URL、query: 検索の条件、title: 見出し、icon、tip: ヒント. 予約済み（/scheduled）は送信前のものなので、別の画面. */
  final case class VirtualList(path: String, query: String, title: String, icon: String, tip: String)
  val virtualLists: List[VirtualList] = List(
    VirtualList("/starred", "is:starred", "スター付き", "⭐", "スターを付けたメール（ゴミ箱・迷惑メールを除く）"),
    VirtualList("/snoozed", "is:snoozed", "スヌーズ中", "⏰", "スヌーズしたメール（指定した日時に受信トレイへ戻ります）")
  )
  def virtualList(path: String): Option[VirtualList] = virtualLists.find(_.path == path)
  val ScheduledPath = "/scheduled"

  def boxLabel(b: MailboxDto): String = b.specialUse.map(SpecialUse.label).getOrElse(b.name)

  // ---------------------------------------------------------------- トースト

  def toast(text: String, error: Boolean = false, undo: Option[() => Unit] = None, undoLabel: String = "元に戻す", ms: Int = 0): Unit = {
    toastSeq += 1
    val t = Toast(toastSeq, text, error, undo, undoLabel)
    toasts := (toasts.v :+ t).takeRight(4)
    dom.window.setTimeout(() => dismiss(t.id), if (ms > 0) ms else if (error) 9000 else if (undo.isDefined) 8000 else 5000)
  }

  def dismiss(id: Int): Unit = toasts := toasts.v.filterNot(_.id == id)

  def error(e: Throwable): Unit = e match {
    case ApiError(401, _) => ()
    case other            => toast(other.getMessage, error = true)
  }

  def report[A](f: Future[A])(ok: A => Unit): Unit = f.onComplete {
    case Success(a) => ok(a)
    case Failure(e) => error(e)
  }

  // ---------------------------------------------------------------- 読み込み

  def loadMailboxes(): Future[List[MailboxDto]] = Api.mailboxes().map { b =>
    mailboxes := b
    b.find(_.specialUse.contains(SpecialUse.Inbox)).foreach(i => Notify.inboxUnread(i.unread))
    b
  }

  def loadSettings(): Unit = Api.get[MailSettingsDto]("/api/me/settings").foreach(settings := _)

  def reloadMe(): Unit = Api.me().foreach(m => me := Some(m))

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
      report(Api.post[UpdatedDto]("/api/messages/snooze", io.circe.syntax.EncoderOps(SnoozeReq(ids, Some(t))).asJson)) { _ =>
        toast(s"${When.label(t.toDate)} までスヌーズしました", undo = Some(() => unsnooze(ids)))
        changed(); after()
      }
    }

  def unsnooze(ids: List[java.util.UUID]): Unit =
    report(Api.post[UpdatedDto]("/api/messages/snooze", io.circe.syntax.EncoderOps(SnoozeReq(ids, None)).asJson)) { _ =>
      toast("スヌーズを解除しました"); changed()
    }

  /** 予約した送信を取り消して、作成画面に戻す. */
  def unschedule(id: java.util.UUID): Unit =
    report(Api.post[ScheduledCancelDto](s"/api/scheduled/$id/cancel")) { c => compose(c.req, c.uploads); toast("送信を取り消しました"); changed() }

  def newMail(to: List[String] = Nil): Unit = {
    val sig = settings.v.signature.trim
    compose(ComposeReq(sendable.headOption.getOrElse(""), to, Nil, Nil, "", if (sig.isEmpty) "" else s"\n\n-- \n$sig", None, Nil, None, None, None))
  }

  def closeDraft(seq: Int): Unit = drafts := drafts.v.filterNot(_.seq == seq)

  def toggleDraft(seq: Int): Unit = drafts := drafts.v.map(d => if (d.seq == seq) d.copy(minimized = !d.minimized) else d)

  // ---------------------------------------------------------------- 自動同期（SSE）

  private var stream: Option[dom.EventSource] = None
  private var pending: Option[Int] = None
  private var lastSync = 0.0

  private def soon(): Unit = if (pending.isEmpty) pending = Some(dom.window.setTimeout(() => { pending = None; sync() }, 300))

  private def sync(): Unit = { lastSync = js.Date.now(); changed() }

  private def open(): Unit = {
    val es = new dom.EventSource("/api/stream")
    es.addEventListener("change", (_: dom.MessageEvent) => soon())
    stream = Some(es)
  }

  def connect(): Unit = if (me.v.isDefined && stream.isEmpty) {
    open()
    dom.window.setInterval(() => {
      // サーバーに断られた（同時接続の上限など）ときは、ブラウザは自動ではつなぎ直さないので、ここでつなぎ直す
      if (stream.exists(_.readyState == dom.EventSource.CLOSED)) open()
      val live = stream.exists(_.readyState == dom.EventSource.OPEN)
      if (dom.document.visibilityState.toString == "visible" && (!live && js.Date.now() - lastSync > 30000)) sync()
    }, 10000)
    dom.document.addEventListener("visibilitychange", (_: dom.Event) => if (dom.document.visibilityState.toString == "visible") soon())
  }

}

/** DOM のイベント・クリップボードの補助. */
object Dom {
  def value(e: dom.Event): String = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
  def checked(e: dom.Event): Boolean = e.target.asInstanceOf[dom.HTMLInputElement].checked
  def copy(text: String, message: String = "コピーしました"): Unit = {
    dom.window.navigator.clipboard.writeText(text)
    Store.toast(message)
  }
}

/** 表示の補助. */
object Fmt {

  private def opts(o: (String, String)*): js.Dynamic = js.Dynamic.literal(o.map((k, v) => k -> (v: js.Any))*)

  def dateTime(t: Timestamp): String =
    t.toDate.asInstanceOf[js.Dynamic].toLocaleString("ja-JP", opts("year" -> "numeric", "month" -> "2-digit", "day" -> "2-digit",
      "hour" -> "2-digit", "minute" -> "2-digit")).asInstanceOf[String]

  /** 日付だけ（2026/11/17）. */
  def date(t: Timestamp): String =
    t.toDate.asInstanceOf[js.Dynamic].toLocaleDateString("ja-JP", opts("year" -> "numeric", "month" -> "2-digit", "day" -> "2-digit")).asInstanceOf[String]

  /** 一覧の日時（今日なら時刻、今年なら月日、それ以前は年月日）. */
  def short(t: Timestamp): String = {
    val d = t.toDate
    val now = new js.Date()
    if (d.toDateString() == now.toDateString()) f"${d.getHours().toInt}%d:${d.getMinutes().toInt}%02d"
    else if (d.getFullYear() == now.getFullYear()) s"${d.getMonth().toInt + 1}月${d.getDate().toInt}日"
    else f"${d.getFullYear().toInt}/${d.getMonth().toInt + 1}%d/${d.getDate().toInt}%d"
  }

  def ago(t: Timestamp): String = {
    val diff = (js.Date.now() - t.toEpochMilli) / 1000
    if (diff < 60) "たった今"
    else if (diff < 3600) s"${(diff / 60).toInt}分前"
    else if (diff < 86400) s"${(diff / 3600).toInt}時間前"
    else if (diff < 86400 * 7) s"${(diff / 86400).toInt}日前"
    else dateTime(t).take(10)
  }

  def bytes(n: Long): String =
    if (n < 1024) s"$n B" else if (n < 1024 * 1024) f"${n / 1024.0}%.1f KB" else if (n < 1024L * 1024 * 1024) f"${n / 1024.0 / 1024.0}%.1f MB"
    else f"${n / 1024.0 / 1024.0 / 1024.0}%.2f GB"

  def addr(a: AddrDto): String = a.name.filter(_.nonEmpty).getOrElse(a.address)
  def addrFull(a: AddrDto): String = a.name.filter(_.nonEmpty).fold(a.address)(n => s"$n <${a.address}>")
  /** 宛先の入力欄の書き方（"名前" <addr>）. */
  def rcpt(a: AddrDto): String = MailText.quoted(a.name, a.address)

  def initials(name: String): String = name.trim.take(1).toUpperCase

  /** 名前から決まる色（アイコンの背景）. */
  def hue(s: String): Int = math.abs(s.hashCode) % 360
}

/** 画面内のダイアログ（ブラウザ標準の confirm・prompt の代わり）. 表示は parts/DialogHost.st. */
final case class DialogReq(message: String, okLabel: String, danger: Boolean, input: Option[RV[String]], choices: List[(String, String)],
                           cancel: Boolean, onOk: String => Unit, onCancel: () => Unit)

object Dialog {
  val current: RV[Option[DialogReq]] = RV(None)
  private def open(r: DialogReq): Unit = current := Some(r)
  def confirm(message: String, ok: String = "OK", danger: Boolean = true)(f: => Unit): Unit =
    open(DialogReq(message, ok, danger, None, Nil, cancel = true, _ => f, () => ()))
  def prompt(message: String, default: String = "", ok: String = "OK")(f: String => Unit): Unit =
    open(DialogReq(message, ok, danger = false, Some(RV(default)), Nil, cancel = true, f, () => ()))
  def choose(message: String, choices: List[(String, String)], ok: String = "OK", danger: Boolean = false)(f: String => Unit): Unit =
    open(DialogReq(message, ok, danger, Some(RV(choices.headOption.map(_._1).getOrElse(""))), choices, cancel = true, f, () => ()))
  def alert(message: String): Unit = open(DialogReq(message, "OK", danger = false, None, Nil, cancel = false, _ => (), () => ()))
  def close(ok: Boolean): Unit = current.v.foreach { r =>
    current := None
    if (ok) r.onOk(r.input.map(_.v).getOrElse("")) else r.onCancel()
  }
}

/** この端末だけの表示の設定（localStorage. 使えないときは既定値）. */
object Prefs {
  def get(key: String, default: String): String =
    scala.util.Try(Option(dom.window.localStorage.getItem("skereru." + key))).toOption.flatten.getOrElse(default)
  def set(key: String, value: String): Unit = scala.util.Try(dom.window.localStorage.setItem("skereru." + key, value))
}

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme {
  val current: RV[String] = RV(Prefs.get("theme", "system"))
  def apply(): Unit = {
    val el = dom.document.documentElement
    if (current.v == "system") el.removeAttribute("data-theme") else el.setAttribute("data-theme", current.v)
  }
  def set(v: String): Unit = { current := v; Prefs.set("theme", v); apply() }
  val options: List[(String, String)] = List("system" -> "🖥 自動", "light" -> "☀️ ライト", "dark" -> "🌙 ダーク")
}

/** オートコンプリート（parts/Combo.st）の選択肢. */
final case class Opt(value: String, label: String, icon: String = "", color: String = "", hint: String = "")

object Opt {
  def of(xs: List[(String, String)]): List[Opt] = xs.map((v, l) => Opt(v, l))
  def names(xs: List[String]): List[Opt] = xs.map(x => Opt(x, x))
}

/** 日時の選択（予約送信・スヌーズ）. 「明日の朝」などの候補と、日時の入力. */
object When {
  private def at(d: js.Date, days: Int, hour: Int): js.Date = {
    val x = new js.Date(d.getFullYear().toInt, d.getMonth().toInt, d.getDate().toInt + days, hour, 0, 0, 0)
    x
  }

  /** 候補（ラベルと日時）. 今より後のものだけ. */
  def choices(): List[(String, js.Date)] = {
    val now = new js.Date()
    val wd = now.getDay().toInt // 0 = 日曜
    val toSat = (6 - wd + 7) % 7 match { case 0 => 7; case n => n }
    val toMon = (1 - wd + 7) % 7 match { case 0 => 7; case n => n }
    List(
      "今日の夕方（18:00）" -> at(now, 0, 18),
      "明日の朝（8:00）" -> at(now, 1, 8),
      "明日の午後（13:00）" -> at(now, 1, 13),
      "今週末（土曜 8:00）" -> at(now, toSat, 8),
      "来週（月曜 8:00）" -> at(now, toMon, 8)
    ).filter(_._2.getTime() > now.getTime() + 60000)
  }

  def label(d: js.Date): String =
    d.asInstanceOf[js.Dynamic].toLocaleString("ja-JP", js.Dynamic.literal(month = "numeric", day = "numeric", weekday = "short",
      hour = "2-digit", minute = "2-digit")).asInstanceOf[String]

  private def iso(d: js.Date): String = d.toISOString()

  /** 日時を選んでもらう（候補か、入力）. 選んだら f に渡す. */
  def pick(title: String)(f: Timestamp => Unit): Unit = {
    val cs = choices()
    Dialog.choose(title, cs.map((l, d) => iso(d) -> s"$l ${label(d)}") :+ ("custom" -> "日時を指定…")) {
      case "custom" =>
        val t = at(new js.Date(), 1, 9)
        def two(n: Double) = f"${n.toInt}%02d"
        val init = s"${t.getFullYear().toInt}-${two(t.getMonth() + 1)}-${two(t.getDate())} 09:00"
        Dialog.prompt("日時を入力してください（例: 2026-11-20 09:00）", init) { v =>
          val d = new js.Date(v.trim.replace(" ", "T"))
          if (d.getTime().isNaN || d.getTime() <= js.Date.now()) Dialog.alert("今より後の日時を、2026-11-20 09:00 の形で入力してください")
          else f(Timestamp(iso(d)))
        }
      case v => f(Timestamp(v))
    }
  }
}
