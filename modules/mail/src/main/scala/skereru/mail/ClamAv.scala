package skereru.mail

import java.io.{DataInputStream, DataOutputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets

/** ClamAV（clamd の INSTREAM）。 */
final class ClamAv(host: String, port: Int) {

  /** ウイルスの名前（見つからなければ None）。つながらなければ例外。 */
  def scan(data: Array[Byte]): Option[String] = {
    val s = new Socket(host, port)
    try {
      s.setSoTimeout(60_000)
      val out = new DataOutputStream(s.getOutputStream)
      out.write("zINSTREAM\u0000".getBytes(StandardCharsets.US_ASCII))
      data.grouped(64 * 1024).foreach { c => out.writeInt(c.length); out.write(c) }
      out.writeInt(0)
      out.flush()
      val reply = new String(
        new DataInputStream(s.getInputStream).readAllBytes(),
        StandardCharsets.US_ASCII
      ).replace("\u0000", "").trim
      if reply.endsWith("OK") then None
      else if reply.endsWith("FOUND") then Some(reply.stripPrefix("stream:").stripSuffix("FOUND").trim)
      else throw new RuntimeException(s"clamd: $reply")
    } finally s.close()
  }
}
