package skereru.web

import skereru.shared.*

import scala.scalajs.js

/** 表示の補助. */
object Fmt {

  private def opts(o: (String, String)*): js.Dynamic = js.Dynamic.literal(o.map((k, v) => k -> (v: js.Any))*)

  def dateTime(t: Timestamp): String =
    t.toDate.asInstanceOf[js.Dynamic].toLocaleString(
      "ja-JP",
      opts(
        "year" -> "numeric",
        "month" -> "2-digit",
        "day" -> "2-digit",
        "hour" -> "2-digit",
        "minute" -> "2-digit"
      )
    ).asInstanceOf[String]

  /** 日付だけ（2026/11/17）. */
  def date(t: Timestamp): String =
    t.toDate.asInstanceOf[js.Dynamic].toLocaleDateString(
      "ja-JP",
      opts("year" -> "numeric", "month" -> "2-digit", "day" -> "2-digit")
    ).asInstanceOf[String]

  /** 一覧の日時（今日なら時刻、今年なら月日、それ以前は年月日）. */
  def short(t: Timestamp): String = {
    val d = t.toDate
    val now = new js.Date()
    if (d.toDateString() == now.toDateString()) f"${d.getHours().toInt}%d:${d.getMinutes().toInt}%02d"
    else if (d.getFullYear() == now.getFullYear()) s"${d.getMonth().toInt + 1}月${d.getDate().toInt}日"
    else f"${d.getFullYear().toInt}/${d.getMonth().toInt + 1}%d/${d.getDate().toInt}%d"
  }

  def ago(t: Timestamp): String = {
    val diff = (js.Date.now() - t.toEpochMilli) / 1000
    if (diff < 60) "たった今"
    else if (diff < 3600) s"${(diff / 60).toInt}分前"
    else if (diff < 86400) s"${(diff / 3600).toInt}時間前"
    else if (diff < 86400 * 7) s"${(diff / 86400).toInt}日前"
    else dateTime(t).take(10)
  }

  def bytes(n: Long): String =
    if (n < 1024) s"$n B"
    else if (n < 1024 * 1024) f"${n / 1024.0}%.1f KB"
    else if (n < 1024L * 1024 * 1024) f"${n / 1024.0 / 1024.0}%.1f MB"
    else f"${n / 1024.0 / 1024.0 / 1024.0}%.2f GB"

  def addr(a: AddrDto): String = a.name.filter(_.nonEmpty).getOrElse(a.address)
  def addrFull(a: AddrDto): String = a.name.filter(_.nonEmpty).fold(a.address)(n => s"$n <${a.address}>")

  /** 宛先の入力欄の書き方（"名前" <addr>）. */
  def rcpt(a: AddrDto): String = MailText.quoted(a.name, a.address)

  def initials(name: String): String = name.trim.take(1).toUpperCase

  /** 名前から決まる色（アイコンの背景）. */
  def hue(s: String): Int = math.abs(s.hashCode) % 360
}
