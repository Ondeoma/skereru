package skereru.core

import java.net.InetAddress

/** 迷惑メールの判定に使う材料（受信時に集めたもの）。 */
final case class SpamInput(
  msg: Array[Byte],
  root: MimePart,
  ip: Option[InetAddress],
  helo: String,
  reverseName: Option[String],
  mailFrom: String,
  spf: SpfResult,
  dkim: List[DkimResult],
  dmarc: DmarcResult,
  dnsblHits: List[String],
  bayes: Option[Double],
  claimsLocalDomain: Boolean,
  virus: Option[String],
  now: java.time.Instant)

final case class SpamHit(
  rule: String,
  score: Double,
  description: String)

enum SpamAction {
  case Deliver, Junk, Reject
}

final case class SpamVerdict(
  score: Double,
  hits: List[SpamHit],
  action: SpamAction) {
  def summary: String = hits.map(h => f"${h.rule}=${h.score}%.1f").mkString(" ")
}

final case class SpamThresholds(
  junk: Double = 5.0,
  reject: Double = 15.0)
