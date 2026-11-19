package skereru.mail

import skereru.db.*

import scala.collection.mutable.ArrayBuffer

/** 選択中のメールボックスの状態（セッションから見えている UID の並びとフラグ）。 */
final class Selected(
  val box: Mailbox,
  val readOnly: Boolean) {
  val uids = ArrayBuffer.empty[Long]
  val flags = scala.collection.mutable.HashMap.empty[Long, List[String]]
  var modseq: Long = box.highestModseq
  def exists: Int = uids.size
  def maxUid: Long = uids.lastOption.getOrElse(0L)

  /** UID の連番（1 始まり）。UID は昇順なので二分探索する。 */
  def seqOf(
    uid: Long
  ): Int = {
    var lo = 0
    var hi = uids.size - 1
    var found = -1
    while lo <= hi && found < 0 do {
      val mid = (lo + hi) >>> 1
      val v = uids(mid)
      if v == uid then found = mid + 1
      else if v < uid then lo = mid + 1
      else hi = mid - 1
    }
    found
  }
}
