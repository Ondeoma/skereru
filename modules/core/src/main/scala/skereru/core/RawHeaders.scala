package skereru.core

import scala.collection.mutable.ArrayBuffer

object RawHeaders {
  def split(msg: Array[Byte]): (List[RawHeader], Int) = {
    val end = Mime.headerEnd(msg, 0, msg.length)
    val text = Mime.latin1(msg, 0, end)
    val out = ArrayBuffer.empty[RawHeader]
    val lines = text.split("(?<=\n)")
    lines.foreach { line =>
      if (line.startsWith(" ") || line.startsWith("\t")) && out.nonEmpty then {
        val last = out.remove(out.length - 1)
        out += last.copy(raw = last.raw + line)
      } else if line.trim.nonEmpty && line.contains(':') then out += RawHeader(line.takeWhile(_ != ':').trim, line)
    }
    (out.toList, end)
  }

  /** keep に合うヘッダーだけを残したメッセージ（本文はそのまま）。何も消さなければ元のバイト列を返す。 */
  def filter(raw: Array[Byte])(keep: RawHeader => Boolean): Array[Byte] = {
    val (hs, bodyStart) = split(raw)
    val kept = hs.filter(keep)
    if kept.size == hs.size then raw
    else
      kept.map(_.raw).mkString.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1) ++ "\r\n".getBytes ++
        raw.drop(bodyStart)
  }
}
