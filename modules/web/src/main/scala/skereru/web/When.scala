package skereru.web

import skereru.shared.*

import scala.scalajs.js

/** 日時の選択（予約送信・スヌーズ）. 「明日の朝」などの候補と、日時の入力. */
object When {
  private def at(
    d: js.Date,
    days: Int,
    hour: Int
  ): js.Date = {
    val x = new js.Date(d.getFullYear().toInt, d.getMonth().toInt, d.getDate().toInt + days, hour, 0, 0, 0)
    x
  }

  /** 候補（ラベルと日時）. 今より後のものだけ. */
  def choices(): List[(String, js.Date)] = {
    val now = new js.Date()
    val wd = now.getDay().toInt // 0 = 日曜
    val toSat = (6 - wd + 7) % 7 match { case 0 => 7; case n => n }
    val toMon = (1 - wd + 7) % 7 match { case 0 => 7; case n => n }
    List(
      "今日の夕方（18:00）" -> at(now, 0, 18),
      "明日の朝（8:00）" -> at(now, 1, 8),
      "明日の午後（13:00）" -> at(now, 1, 13),
      "今週末（土曜 8:00）" -> at(now, toSat, 8),
      "来週（月曜 8:00）" -> at(now, toMon, 8)
    ).filter(_._2.getTime() > now.getTime() + 60000)
  }

  def label(
    d: js.Date
  ): String =
    d.asInstanceOf[js.Dynamic].toLocaleString(
      "ja-JP",
      js.Dynamic.literal(
        month = "numeric",
        day = "numeric",
        weekday = "short",
        hour = "2-digit",
        minute = "2-digit"
      )
    ).asInstanceOf[String]

  private def iso(
    d: js.Date
  ): String = d.toISOString()

  /** 日時を選んでもらう（候補か、入力）. 選んだら f に渡す. */
  def pick(
    title: String
  )(
    f: Timestamp => Unit
  ): Unit = {
    val cs = choices()
    Dialog.choose(
      title,
      cs.map(
        (
          l,
          d
        ) => iso(d) -> s"$l ${label(d)}"
      ) :+ ("custom" -> "日時を指定…")
    ) {
      case "custom" =>
        val t = at(new js.Date(), 1, 9)
        def two(
          n: Double
        ) = f"${n.toInt}%02d"
        val init = s"${t.getFullYear().toInt}-${two(t.getMonth() + 1)}-${two(t.getDate())} 09:00"
        Dialog.prompt("日時を入力してください（例: 2026-11-20 09:00）", init) { v =>
          val d = new js.Date(v.trim.replace(" ", "T"))
          if (d.getTime().isNaN || d.getTime() <= js.Date.now()) Dialog.alert("今より後の日時を、2026-11-20 09:00 の形で入力してください")
          else f(Timestamp(iso(d)))
        }
      case v => f(Timestamp(v))
    }
  }
}
