package skereru.web

import org.scalajs.dom

/**
 * ツールチップ: title の付いた要素にカーソルを重ねる（またはキーボードでフォーカスする）と、すぐにヒントを出す.
 * ブラウザ標準の title のツールチップは出るまでに時間がかかり気付きにくいので、表示中は title を data-tip に移して標準のものを止める.
 * 画面側は今までどおり title を書くだけでよい. 後から足された要素にも効く（イベントの委譲）.
 */
object Tooltip {

  private val DelayMs = 250

  private var tip: Option[dom.HTMLElement] = None
  private var current: Option[dom.Element] = None
  private var timer = 0

  private def el(): dom.HTMLElement = tip.getOrElse {
    val t = dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
    t.className = "tooltip"
    t.setAttribute("role", "tooltip")
    dom.document.body.append(t)
    tip = Some(t)
    t
  }

  private def target(node: dom.EventTarget): Option[dom.Element] = node match {
    case e: dom.Element => Option(e.closest("[title], [data-tip]"))
    case _              => None
  }

  private def show(t: dom.Element): Unit = {
    val text = Option(t.getAttribute("data-tip")).getOrElse("")
    if (text.nonEmpty) {
      val box = el()
      box.textContent = text
      box.classList.add("on")
      val r = t.getBoundingClientRect()
      val w = box.offsetWidth
      val h = box.offsetHeight
      // ふだんは下に出す. 画面の下に収まらなければ上に出す. 左右は画面からはみ出さないようにする
      val top = if (r.bottom + 6 + h > dom.window.innerHeight - 4) r.top - h - 6 else r.bottom + 6
      val left = math.min(math.max(4, r.left + r.width / 2 - w / 2), dom.window.innerWidth - w - 4)
      box.style.top = s"${math.round(top)}px"
      box.style.left = s"${math.round(left)}px"
    }
  }

  private def enter(t: dom.Element): Unit = if (!current.contains(t)) {
    leave()
    current = Some(t)
    // 標準のツールチップを止める（data-tip に移す）
    if (t.hasAttribute("title")) {
      t.setAttribute("data-tip", t.getAttribute("title"))
      t.removeAttribute("title")
    }
    if (!t.hasAttribute("aria-label") && t.textContent.trim.isEmpty) t.setAttribute("aria-label", t.getAttribute("data-tip"))
    timer = dom.window.setTimeout(() => if (current.contains(t)) show(t), DelayMs)
  }

  private def leave(): Unit = {
    dom.window.clearTimeout(timer)
    current.foreach { c =>
      // 画面側が title を書き換えることがあるので、戻しておく
      if (!c.hasAttribute("title") && c.hasAttribute("data-tip")) c.setAttribute("title", c.getAttribute("data-tip"))
    }
    current = None
    tip.foreach(_.classList.remove("on"))
  }

  /** 画面の起動時に1度呼ぶ. */
  def install(): Unit = {
    // タッチだけの端末ではカーソルを重ねられないので出さない（長押しのメニューと重ならないように）
    val hover = dom.window.matchMedia("(hover: hover)")
    val doc = dom.document
    doc.addEventListener("mouseover", (ev: dom.MouseEvent) => if (hover.matches) target(ev.target) match {
      case Some(t) => enter(t)
      case None    => if (current.isDefined) leave()
    })
    doc.addEventListener("focusin", (ev: dom.FocusEvent) => target(ev.target).filter(_.matches(":focus-visible")).foreach(enter))
    doc.addEventListener("focusout", (_: dom.FocusEvent) => leave())
    // カーソルが画面の外に出たとき
    doc.addEventListener("mouseout", (ev: dom.MouseEvent) => if (ev.relatedTarget == null) leave())
    // 押したら消す（メニューが開く・画面が変わるなど）
    doc.addEventListener("mousedown", (_: dom.MouseEvent) => leave(), true)
    doc.addEventListener("keydown", (ev: dom.KeyboardEvent) => if (ev.key == "Escape") leave())
    dom.window.addEventListener("scroll", (_: dom.Event) => leave(), true)
  }

}
