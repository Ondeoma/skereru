package skereru.core

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.safety.{Cleaner, Safelist}

import scala.jdk.CollectionConverters.*

/** HTML メールの無害化（スクリプト・フォーム・イベント属性・外部の読み込みを取り除く）とテキスト化。 */
object Html:

  private val safelist: Safelist =
    Safelist.relaxed()
      .addTags("center", "font", "hr", "span", "div", "small", "big", "s", "strike", "del", "ins", "abbr", "address", "bdi", "bdo", "mark", "wbr")
      .addAttributes(":all", "style", "align", "valign", "dir", "lang", "title", "width", "height", "bgcolor", "color")
      .addAttributes("table", "border", "cellpadding", "cellspacing", "summary", "background")
      .addAttributes("td", "colspan", "rowspan", "nowrap", "background")
      .addAttributes("th", "colspan", "rowspan", "nowrap", "scope")
      .addAttributes("font", "face", "size", "color")
      .addAttributes("img", "alt", "src", "border", "hspace", "vspace")
      .addAttributes("a", "href", "name", "target", "rel")
      .addProtocols("a", "href", "http", "https", "mailto", "tel")
      .addProtocols("img", "src", "http", "https", "cid", "data")
      .preserveRelativeLinks(false)

  /** 表示用の HTML を作る。
    * cid: の画像は cidUrl で自分のサーバーの URL に、外部の画像は allowRemote のときだけ残す。戻り値の2つ目は外部の画像があったか。 */
  def sanitize(html: String, cidUrl: String => Option[String], allowRemote: Boolean): (String, Boolean) =
    val dirty = Jsoup.parse(html)
    // <style> の中身は Safelist で消えるので、残す前に退避しておく（外部の読み込みは除く）
    val styles = dirty.select("style").asScala.map(_.data()).map(stripCssUrls(_, allowRemote)).mkString("\n")
    val clean = new Cleaner(safelist).clean(dirty)
    var remote = false
    clean.select("img").asScala.foreach { img =>
      val src = img.attr("src").trim
      if src.toLowerCase.startsWith("cid:") then
        cidUrl(src.drop(4)) match
          case Some(u) => img.attr("src", u)
          case None    => img.removeAttr("src")
      else if src.toLowerCase.startsWith("data:") then
        if !src.toLowerCase.matches("^data:image/(png|jpeg|jpg|gif|webp);base64,.*") then img.removeAttr("src")
      else if src.nonEmpty then
        remote = true
        if !allowRemote then
          img.removeAttr("src")
          img.attr("data-blocked", "1")
    }
    // 背景画像の属性（外部の読み込み）
    clean.select("[background]").asScala.foreach { e =>
      remote = true
      if !allowRemote then e.removeAttr("background")
    }
    clean.select("[style]").asScala.foreach { e =>
      val st = unescapeCss(e.attr("style"))
      val lower = st.toLowerCase
      if lower.contains("url(") || lower.contains("image-set(") then remote = true
      if lower.contains("expression(") || lower.contains("-moz-binding") then e.removeAttr("style")
      else e.attr("style", stripCssUrls(st, allowRemote))
    }
    clean.select("a[href]").asScala.foreach { a =>
      a.attr("target", "_blank")
      a.attr("rel", "noopener noreferrer nofollow")
    }
    if styles.toLowerCase.contains("url(") then remote = true
    clean.outputSettings().prettyPrint(false)
    // 本文の中身だけを返す（<style> は先頭に置く。< を含む CSS は捨てる）
    val css = if styles.trim.isEmpty || styles.contains("<") then "" else s"<style>${styles}</style>"
    (css + clean.body().html(), remote)

  /** CSS の url(…) と @import を取り除く（allowRemote のときは https の url だけ残す）。 */
  private def stripCssUrls(css: String, allowRemote: Boolean): String =
    // CSS のエスケープ（u\72l( など）は外部の読み込みを隠すのに使われるので、戻してから調べる
    val noImport = unescapeCss(css).replaceAll("(?i)@import[^;]*;?", "")
    val noSet = if allowRemote then noImport else replaceCalls(noImport, ImageSet, _ => "none")
    val safe = noSet.replaceAll("(?i)expression\\s*\\(", "x(").replaceAll("(?i)behavior\\s*:", "x:").replaceAll("(?i)-moz-binding", "x")
    replaceCalls(safe, Url, call =>
      val inner = call.drop(4).dropRight(1).trim.stripPrefix("\"").stripPrefix("'")
      if allowRemote && inner.toLowerCase.startsWith("https://") then call else "none")

  private val Url = java.util.regex.Pattern.compile("(?i)url\\(")
  private val ImageSet = java.util.regex.Pattern.compile("(?i)(-webkit-)?image-set\\s*\\(")

  /** name( … ) の呼び出しを repl で置き換える。閉じ括弧がなければ、残りすべてを1つの呼び出しとみなす。
    * 正規表現の "url\\([^)]*\\)" は、閉じない "url(" を並べると計算が二乗になるので、前から1回だけ走査する。 */
  private def replaceCalls(css: String, name: java.util.regex.Pattern, repl: String => String): String =
    val m = name.matcher(css)
    val sb = new StringBuilder
    var from = 0
    while from < css.length && m.find(from) do
      val close = css.indexOf(')', m.end())
      val stop = if close < 0 then css.length else close + 1
      sb.append(css.substring(from, m.start())).append(repl(css.substring(m.start(), stop)))
      from = stop
    if from < css.length then sb.append(css.substring(from))
    sb.toString

  /** CSS のエスケープ（\72 や \r）を文字に戻す。 */
  def unescapeCss(css: String): String =
    """\\([0-9a-fA-F]{1,6})\s?|\\(.)""".r.replaceAllIn(css, m =>
      java.util.regex.Matcher.quoteReplacement(
        if m.group(1) != null then scala.util.Try(new String(Character.toChars(Integer.parseInt(m.group(1), 16)))).getOrElse("")
        else m.group(2)))

  /** HTML をテキストにする（段落・改行を保つ）。 */
  def toText(html: String): String =
    val doc: Document = Jsoup.parse(html)
    doc.select("script, style, head").remove()
    val sb = new StringBuilder
    def walk(e: Element): Unit =
      e.childNodes().asScala.foreach {
        case t: org.jsoup.nodes.TextNode => sb.append(t.text())
        case el: Element =>
          val tag = el.tagName().toLowerCase
          if tag == "br" then sb.append('\n')
          else
            val block = Set("p", "div", "tr", "li", "h1", "h2", "h3", "h4", "h5", "h6", "blockquote", "pre", "table", "ul", "ol")(tag)
            if block && sb.nonEmpty && sb.last != '\n' then sb.append('\n')
            if tag == "li" then sb.append("・")
            walk(el)
            if tag == "a" then
              val href = el.attr("href")
              if href.startsWith("http") && href != el.text() && el.text().nonEmpty then sb.append(s" ($href)")
            if block && sb.nonEmpty && sb.last != '\n' then sb.append('\n')
        case _ => ()
      }
    walk(doc.body())
    // 行末の空白を除く（正規表現の "[ \t]+\n" は、空白を並べた本文で計算が二乗になるので、行ごとに後ろから除く）
    sb.toString.split("\n", -1).map(rtrim).mkString("\n").replaceAll("\n{3,}", "\n\n").trim

  private def rtrim(l: String): String =
    var e = l.length
    while e > 0 && (l.charAt(e - 1) == ' ' || l.charAt(e - 1) == '\t' || l.charAt(e - 1) == '\u00a0') do e -= 1
    l.substring(0, e)

  /** テキストを HTML にする（返信の引用・署名用）。 */
  def escape(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;").replace("'", "&#39;")

  def fromText(s: String): String = escape(s).replace("\r\n", "\n").replace("\n", "<br>")
