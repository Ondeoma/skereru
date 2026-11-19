package skereru.core

import java.nio.charset.StandardCharsets

/** IMAP のメールボックス名の修正 UTF-7（RFC 3501 5.1.3）。 */
object MUtf7 {
  def encode(s: String): String = {
    val out = new StringBuilder
    val buf = new StringBuilder
    def flush(): Unit = if buf.nonEmpty then {
      val b = buf.toString.getBytes(StandardCharsets.UTF_16BE)
      out.append('&').append(java.util.Base64.getEncoder.withoutPadding().encodeToString(b).replace(
        '/',
        ','
      )).append('-')
      buf.clear()
    }
    s.foreach { c =>
      if c >= 0x20 && c <= 0x7e then {
        flush()
        if c == '&' then out.append("&-") else out.append(c)
      } else buf.append(c)
    }
    flush()
    out.toString
  }

  def decode(s: String): String = {
    val out = new StringBuilder
    var i = 0
    while i < s.length do
      if s(i) == '&' then {
        val j = s.indexOf('-', i)
        if j < 0 then { out.append(s.substring(i)); i = s.length }
        else if j == i + 1 then { out.append('&'); i = j + 1 }
        else {
          val enc = s.substring(i + 1, j).replace(',', '/')
          scala.util.Try(java.util.Base64.getDecoder.decode(enc + "=" * ((4 - enc.length % 4) % 4))) match {
            case scala.util.Success(b) => out.append(new String(b, StandardCharsets.UTF_16BE))
            case _ => out.append(s.substring(i, j + 1))
          }
          i = j + 1
        }
      } else { out.append(s(i)); i += 1 }
    out.toString
  }
}
