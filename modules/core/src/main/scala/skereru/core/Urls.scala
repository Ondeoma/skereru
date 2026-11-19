package skereru.core

/** URL の抜き出し。 */
object Urls {
  val Shorteners = Set(
    "bit.ly",
    "tinyurl.com",
    "t.co",
    "goo.gl",
    "ow.ly",
    "is.gd",
    "buff.ly",
    "rebrand.ly",
    "cutt.ly",
    "shorturl.at",
    "x.gd"
  )
  private val Re = """https?://[^\s<>"')\]]+""".r
  def find(
    text: String
  ): List[String] = Re.findAllIn(text).take(500).toList
  def host(
    u: String
  ): Option[String] = scala.util.Try(java.net.URI.create(u).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)
}
