package skereru.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import io.github.ondeoma.scalactive.routes.Router
import org.scalajs.dom
import skereru.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** 新着の通知: タブのタイトルの未読の数と、ブラウザの通知（この端末で有効にしたときだけ）. */
object Notify {
  private val Key = "skereru.notify"
  private var lastUnread: Option[Int] = None

  // 無いブラウザもあるので window のプロパティとして読む（js.Dynamic.global だと ReferenceError になる）
  private def api: js.Dynamic = dom.window.asInstanceOf[js.Dynamic].Notification
  def supported: Boolean = !js.isUndefined(api)
  private def permission: String = if (supported) api.permission.asInstanceOf[String] else "denied"

  /** この端末で通知を出すか（localStorage. 読めなければ出さない）. */
  def enabled: Boolean = scala.util.Try(dom.window.localStorage.getItem(Key) == "on").getOrElse(false) &&
    permission == "granted"

  val state: RV[String] = RV(if (enabled) "on" else "off")

  /** 有効にする（ブラウザが許可を求める. 許可されなければ有効にしない）. */
  def enable(): Unit =
    if (!supported) Store.toast("このブラウザは通知に対応していません", error = true)
    else api.requestPermission().asInstanceOf[js.Promise[String]].toFuture.foreach { p =>
      if (p == "granted") {
        scala.util.Try(dom.window.localStorage.setItem(Key, "on")); state := "on"; Store.toast("新着の通知を有効にしました")
      } else Store.toast("ブラウザで通知が許可されていません（ブラウザのサイトの設定で許可できます）", error = true)
    }

  def disable(): Unit = { scala.util.Try(dom.window.localStorage.removeItem(Key)); state := "off" }

  /** 受信トレイの未読の数が変わったとき（メールボックスを読み直すたびに呼ぶ）. */
  def inboxUnread(n: Int): Unit = {
    badge(n)
    val grew = lastUnread.exists(n > _)
    lastUnread = Some(n)
    if (grew && enabled && dom.document.visibilityState.toString != "visible")
      Store.special(SpecialUse.Inbox).foreach { b =>
        Api.messages(Some(b.id), "", 0, 1).foreach(_.items.headOption.filterNot(_.seen).foreach { m =>
          val n = js.Dynamic.newInstance(api)(
            m.from.map(Fmt.addr).getOrElse("新しいメール"),
            js.Dynamic.literal(
              body = if (m.subject.isEmpty) "(件名なし)" else m.subject,
              icon = "/favicon.svg",
              tag = "skereru-new"
            )
          )
          n.onclick = (() => { dom.window.focus(); Router.go(s"/m/${m.id}") }): js.Function0[Unit]
        })
      }
  }

  // ---------------------------------------------------------------- タブのタイトルの未読の数

  private var count = 0
  private val Prefix = "^\\(\\d+\\) ".r

  private def apply(): Unit = {
    val base = Prefix.replaceFirstIn(dom.document.title, "")
    val want = if (count > 0) s"($count) $base" else base
    if (dom.document.title != want) dom.document.title = want
  }

  private def badge(n: Int): Unit = { count = n; apply() }

  /** 画面がタイトルを変えたら、未読の数を付け直す. */
  def install(): Unit = {
    val obs = new dom.MutationObserver((_, _) => apply())
    Option(dom.document.querySelector("title")).foreach(t =>
      obs.observe(t, new dom.MutationObserverInit { childList = true; characterData = true; subtree = true })
    )
  }
}
