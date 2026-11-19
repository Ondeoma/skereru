package skereru.web

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** Tiptap のエディタ（frontend/src/editor.js. ライブラリを呼ぶ所だけ JS）. */
@js.native
@JSGlobal("skereruEditor")
private object TiptapFacade extends js.Object {
  def create(el: dom.Element, opts: js.Object): Tiptap = js.native
}

@js.native
private trait Tiptap extends js.Object {
  def run(name: String, arg: js.UndefOr[String] = js.undefined): Unit = js.native
  def isActive(name: String): Boolean = js.native
  def linkHref(): String = js.native
  def getHTML(): String = js.native
  def getText(): String = js.native
  def isEmpty: Boolean = js.native
  def focus(): Unit = js.native
  def destroy(): Unit = js.native
}

/** メールの作成のエディタ（ツールバー・ファイルのドロップ付き）. */
final class EditorHandle private[web] (t: Tiptap, wrap: dom.Element) {
  def getHTML(): String = t.getHTML()

  /** テキストの本文（段落・改行・引用・箇条書きを保つ）. */
  def getText(): String = t.getText().replace(' ', ' ')
  def isEmpty(): Boolean = t.isEmpty
  def focus(): Unit = t.focus()
  def destroy(): Unit = { t.destroy(); wrap.remove() }
}

object Editor {

  // ラベル・ヒント・押したとき・押された状態か. 確認や入力には、ブラウザ標準のダイアログ（window.prompt など）を使わない
  private val buttons: List[(String, String, Tiptap => Unit, Tiptap => Boolean)] = {
    def toggle(label: String, title: String, name: String) =
      (label, title, (t: Tiptap) => t.run(name), (t: Tiptap) => t.isActive(name))
    List(
      toggle("B", "太字", "bold"),
      toggle("I", "斜体", "italic"),
      toggle("U", "下線", "underline"),
      toggle("S", "取り消し線", "strike"),
      toggle("•", "箇条書き", "bulletList"),
      toggle("1.", "番号付き", "orderedList"),
      toggle("❝", "引用", "blockquote"),
      ("🔗", "リンク", askLink, _.isActive("link")),
      ("⌫", "書式を消す", _.run("clearFormat"), _ => false)
    )
  }

  /** リンクの URL を聞く（アプリのダイアログ）. 取り消したときは何もしない. ダイアログの間にフォーカスが外れても、選択範囲はエディタが覚えている. */
  private def askLink(t: Tiptap): Unit =
    Dialog.prompt("リンクの URL（空にすると外す。http・https・mailto）", t.linkHref()) { url =>
      val u = url.trim
      if (u.isEmpty) t.run("unsetLink")
      else if (u.matches("(?i)^(https?:|mailto:).*")) t.run("setLink", u)
    }

  /** onFiles: 本文にドロップされたファイル（添付する）. */
  def create(el: dom.Element,
             html: String,
             placeholder: String,
             onChange: () => Unit,
             onFiles: List[dom.File] => Unit): EditorHandle = {
    def div(cls: String) = {
      val d = dom.document.createElement("div").asInstanceOf[dom.HTMLElement]; d.className = cls; d
    }
    val wrap = div("mail-editor")
    val bar = div("mail-editor-bar")
    val area = div("mail-editor-area")
    wrap.append(bar, area)
    el.append(wrap)

    var refresh: () => Unit = () => ()
    val t = TiptapFacade.create(
      area,
      js.Dynamic.literal(
        html = html,
        placeholder = placeholder,
        onUpdate = (() => { onChange(); refresh() }): js.Function0[Unit],
        onSelectionUpdate = (() => refresh()): js.Function0[Unit]
      ).asInstanceOf[js.Object]
    )

    val made = buttons.map {
      (label, title, run, active) =>
        val b = dom.document.createElement("button").asInstanceOf[dom.HTMLButtonElement]
        b.`type` = "button"
        b.textContent = label
        b.title = title
        b.addEventListener("mousedown", (ev: dom.MouseEvent) => ev.preventDefault())
        b.addEventListener("click", (_: dom.MouseEvent) => run(t))
        bar.append(b)
        (b, active)
    }
    refresh = () =>
      made.foreach((b, active) => b.classList.toggle("on", active(t)))

    dropFiles(wrap, onFiles)
    new EditorHandle(t, wrap)
  }

  /**
   * ファイルのドロップで添付する（onFiles に渡す）. 本文に画像などとして入れたり、ブラウザがファイルを開いて
   * 書きかけが消えたりしないよう、エディタより先に（capture で）受け取る. 文字のドラッグ（並べ替え）はそのまま通す.
   */
  private def dropFiles(wrap: dom.Element, onFiles: List[dom.File] => Unit): Unit = {
    def hasFiles(ev: dom.DragEvent): Boolean =
      ev.dataTransfer != null &&
        Option(ev.dataTransfer.types).exists(ts => (0 until ts.length).exists(ts(_) == "Files"))
    var depth = 0
    wrap.addEventListener(
      "dragenter",
      (ev: dom.DragEvent) => if (hasFiles(ev)) { depth += 1; wrap.classList.add("dropping") },
      true
    )
    wrap.addEventListener(
      "dragleave",
      (ev: dom.DragEvent) =>
        if (hasFiles(ev)) { depth -= 1; if (depth <= 0) { depth = 0; wrap.classList.remove("dropping") } },
      true
    )
    wrap.addEventListener(
      "dragover",
      (ev: dom.DragEvent) =>
        if (hasFiles(ev)) {
          ev.preventDefault()
          ev.dataTransfer.dropEffect = dom.DataTransferDropEffectKind.copy
        },
      true
    )
    wrap.addEventListener(
      "drop",
      (ev: dom.DragEvent) =>
        if (hasFiles(ev)) {
          ev.preventDefault()
          ev.stopPropagation()
          depth = 0
          wrap.classList.remove("dropping")
          val fs = Option(ev.dataTransfer.files).map(l => (0 until l.length).map(l(_)).toList).getOrElse(Nil)
          if (fs.nonEmpty) onFiles(fs)
        },
      true
    )
  }

}
