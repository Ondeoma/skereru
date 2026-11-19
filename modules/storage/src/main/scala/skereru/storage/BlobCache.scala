package skereru.storage

/** 最近読んだ本体を覚えておく（IMAP は同じメッセージを何度も部分的に読む）。 */
final class BlobCache(maxBytes: Long) {
  private val map = new java.util.LinkedHashMap[String, Array[Byte]](256, 0.75f, true)
  private var size = 0L
  def get(id: String): Option[Array[Byte]] = synchronized(Option(map.get(id)))
  def put(id: String, b: Array[Byte]): Unit = synchronized {
    if b.length <= maxBytes / 4 && !map.containsKey(id) then {
      map.put(id, b); size += b.length
      val it = map.entrySet().iterator()
      while size > maxBytes && it.hasNext do {
        val e = it.next(); size -= e.getValue.length; it.remove()
      }
    }
  }
}
