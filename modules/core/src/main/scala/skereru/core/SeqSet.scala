package skereru.core

import scala.collection.mutable.ArrayBuffer

/** シーケンス集合（1:3,5,7:*）。-1 は「*」（最大の番号）。 */
final case class SeqSet(ranges: List[(Long, Long)]) {
  private def resolve(n: Long, max: Long) = if n == -1 then max else n

  def contains(n: Long, max: Long): Boolean = ranges.exists {
    (a0, b0) =>
      {
        val a = resolve(a0, max); val b = resolve(b0, max)
        n >= math.min(a, b) && n <= math.max(a, b)
      }
  }

  /** 最大値までの番号の範囲（DB の問い合わせ用）。 */
  def bounds(max: Long): List[(Long, Long)] = ranges.map {
    (a0, b0) =>
      {
        val a = resolve(a0, max); val b = resolve(b0, max)
        (math.min(a, b), math.max(a, b))
      }
  }

  /** 「*」を含むか（UID の範囲で、最大の UID を必ず含めるため）。 */
  def hasStar: Boolean = ranges.exists((a, b) => a == -1 || b == -1)
}

object SeqSet {
  val MaxRanges = 2000
  def parse(s: String): SeqSet = {
    if s.isEmpty then throw ImapSyntax("sequence set is empty")
    def num(x: String): Long =
      if x == "*" then -1
      else x.toLongOption.filter(_ > 0).getOrElse(throw ImapSyntax(s"invalid sequence number: $x"))
    // 範囲の数に上限を付ける（1,3,5,… を並べて、判定や SQL を巨大にさせない）
    if s.count(_ == ',') >= MaxRanges then throw ImapSyntax("too many ranges in sequence set")
    SeqSet(s.split(',').toList.map { p =>
      p.split(':') match {
        case Array(a) => (num(a), num(a))
        case Array(a, b) => (num(a), num(b))
        case _ => throw ImapSyntax(s"invalid sequence set: $s")
      }
    })
  }

  /** UID の並びを短く書く（1:3,5）。 */
  def format(nums: Seq[Long]): String =
    if nums.isEmpty then ""
    else {
      val sorted = nums.distinct.sorted
      val out = ArrayBuffer.empty[String]
      var start = sorted.head
      var prev = sorted.head
      sorted.tail.foreach { n =>
        if n == prev + 1 then prev = n
        else {
          out += (if start == prev then s"$start" else s"$start:$prev"); start = n; prev = n
        }
      }
      out += (if start == prev then s"$start" else s"$start:$prev")
      out.mkString(",")
    }
}
