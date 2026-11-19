package skereru.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import io.github.ondeoma.scalactive.routes.Router
import org.scalajs.dom

/**
 * キーボードのショートカット（Gmail に近いもの）. 画面ごとに handlers を登録し、離れるときに外す.
 * 入力欄・ダイアログ・修飾キー（Ctrl・⌘・Alt）のときは使わない.
 */
object Shortcuts {

  /** 説明（? で出す一覧）. */
  val help: List[(String, String)] = List(
    "c" -> "新しいメールを作成",
    "/" -> "検索",
    "j / k" -> "次・前のメール（一覧）",
    "o / Enter" -> "開く",
    "x" -> "選ぶ（一覧）",
    "u" -> "一覧に戻る",
    "e" -> "アーカイブ",
    "#" -> "削除",
    "s" -> "スターを付ける・外す",
    "b" -> "スヌーズ",
    "r / a / f" -> "返信・全員に返信・転送（スレッド）",
    "g i / g s / g t" -> "受信トレイ・スター付き・送信済みへ",
    "?" -> "この一覧"
  )

  val showHelp: RV[Boolean] = RV(false)
  private var handlers: Map[String, () => Unit] = Map.empty
  private var pendingG = false

  /** 画面のショートカットを登録する（同じキーは後から登録したものが勝つ）. 外すための関数を返す. */
  def register(keys: (String, () => Unit)*): () => Unit = {
    handlers = handlers ++ keys
    () => handlers = handlers -- keys.map(_._1)
  }

  private def typing(e: dom.KeyboardEvent): Boolean = e.target match {
    case el: dom.HTMLElement =>
      val tag = el.tagName.toLowerCase
      tag == "input" || tag == "textarea" || tag == "select" || el.isContentEditable
    case _ => false
  }

  private def go(path: String): Unit = Router.go(path)

  def install(): Unit = dom.document.addEventListener(
    "keydown",
    (e: dom.KeyboardEvent) =>
      if (!e.ctrlKey && !e.metaKey && !e.altKey && !typing(e) && Dialog.current.v.isEmpty) {
        val k = e.key
        if (pendingG) {
          pendingG = false
          k match {
            case "i" => e.preventDefault(); go("/mail/inbox")
            case "s" => e.preventDefault(); go("/starred")
            case "t" => e.preventDefault(); go("/mail/sent")
            case _ => ()
          }
        } else k match {
          case "?" => e.preventDefault(); showHelp := !showHelp.v
          case "Escape" if showHelp.v => showHelp := false
          case "g" => pendingG = true; dom.window.setTimeout(() => pendingG = false, 1500)
          case "c" => e.preventDefault(); Store.newMail()
          case "/" =>
            e.preventDefault()
            Option(dom.document.querySelector(".search input")).foreach(_.asInstanceOf[dom.HTMLInputElement].focus())
          case other => handlers.get(other).foreach { f => e.preventDefault(); f() }
        }
      }
  )
}
