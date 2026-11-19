package skereru.core

/** 日時の解析（Date ヘッダー）。 */
object Dates {
  private val fmt = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME

  def parseRfc5322(raw: String): Option[java.time.Instant] = {
    val s = raw.replaceAll("""\([^)]*\)""", "").replaceAll("\\s+", " ").trim
    // 曜日なし・秒なし・2桁の年・タイムゾーン名などの揺れを吸収する
    val noDow = s.replaceFirst("^[A-Za-z]{3},\\s*", "")
    val candidates = List(s, noDow, noDow.replaceFirst("(\\d{1,2}:\\d{2})(\\s)", "$1:00$2"))
    candidates.view.flatMap { c =>
      scala.util.Try(java.time.ZonedDateTime.parse(c, fmt).toInstant).toOption
        .orElse(scala.util.Try(java.time.ZonedDateTime.parse(
          c,
          java.time.format.DateTimeFormatter.ofPattern("d MMM yyyy HH:mm:ss Z", java.util.Locale.ENGLISH)
        ).toInstant).toOption)
        .orElse(scala.util.Try(java.time.ZonedDateTime.parse(
          c.replaceAll(" (UT|GMT|UTC|Z)$", " +0000").replaceAll(" JST$", " +0900").replaceAll(" EST$", " -0500")
            .replaceAll(" EDT$", " -0400").replaceAll(" PST$", " -0800").replaceAll(" PDT$", " -0700"),
          java.time.format.DateTimeFormatter.ofPattern("d MMM yyyy HH:mm:ss Z", java.util.Locale.ENGLISH)
        ).toInstant).toOption)
    }.headOption
  }

  def rfc5322(t: java.time.Instant, zone: java.time.ZoneId): String = fmt.format(t.atZone(zone))
}
