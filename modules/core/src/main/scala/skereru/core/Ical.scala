package skereru.core

import java.time.{Duration, Instant, LocalDate, LocalDateTime, ZoneId, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.collection.mutable.ListBuffer
import scala.util.Try

/** iCalendar のプロパティ（NAME;PARAM=…:VALUE）。raw は折り返しを戻した元の1行。 */
final case class IcalProp(
  name: String,
  params: Map[String, String],
  value: String,
  raw: String)

/** 予定の日時（終日なら日付、そうでなければ時刻）。 */
enum IcalTime {
  case Day(
    date: LocalDate)
  case At(
    instant: Instant)
}

final case class IcalPerson(
  address: String,
  name: Option[String],
  partstat: Option[String])

/** 会議の招待（iMIP。RFC 6047）。method は REQUEST・CANCEL・REPLY など。 */
final case class Invite(
  method: String,
  uid: String,
  summary: String,
  description: String,
  location: String,
  start: IcalTime,
  end: IcalTime,
  organizer: Option[IcalPerson],
  attendees: List[IcalPerson],
  sequence: Int,
  recurring: Boolean,
  props: List[IcalProp]) {
  def allDay: Boolean = start.isInstanceOf[IcalTime.Day]
}

/** iCalendar（RFC 5545）の読み取りと、招待への返事（REPLY）の組み立て。中身は外から届くので、壊れていても例外は出さない。 */
object Ical {

  private val MaxBytes = 1024 * 1024

  /** 折り返し（行頭の空白・タブ）を戻して、行に分ける。 */
  def unfold(
    text: String
  ): List[String] = {
    val out = ListBuffer.empty[String]
    val sb = new StringBuilder
    text.replace("\r\n", "\n").split("\n", -1).foreach { l =>
      if (l.startsWith(" ") || l.startsWith("\t")) && sb.nonEmpty then sb.append(l.drop(1))
      else {
        if sb.nonEmpty then out += sb.toString
        sb.clear(); sb.append(l)
      }
    }
    if sb.nonEmpty then out += sb.toString
    out.toList.filter(_.trim.nonEmpty)
  }

  /** 1行を名前・パラメーター・値に分ける（引用符の中の : ; は区切りにしない）。 */
  def prop(
    line: String
  ): Option[IcalProp] = {
    var quoted = false
    var colon = -1
    var i = 0
    while i < line.length && colon < 0 do {
      val c = line.charAt(i)
      if c == '"' then quoted = !quoted
      else if c == ':' && !quoted then colon = i
      i += 1
    }
    if colon <= 0 then None
    else {
      val head = line.substring(0, colon)
      val parts = ListBuffer.empty[String]
      val sb = new StringBuilder
      quoted = false
      head.foreach { c =>
        if c == '"' then quoted = !quoted
        if c == ';' && !quoted then { parts += sb.toString; sb.clear() }
        else sb.append(c)
      }
      parts += sb.toString
      val params = parts.tail.flatMap { p =>
        p.indexOf('=') match {
          case -1 => None
          case k =>
            Some(p.substring(0, k).trim.toUpperCase -> p.substring(k + 1).trim.stripPrefix("\"").stripSuffix("\""))
        }
      }.toMap
      Some(IcalProp(parts.head.trim.toUpperCase, params, line.substring(colon + 1), line))
    }
  }

  /** TEXT の値のエスケープを戻す。 */
  def unescape(
    v: String
  ): String = {
    val sb = new StringBuilder
    var i = 0
    while i < v.length do {
      val c = v.charAt(i)
      if c == '\\' && i + 1 < v.length then {
        v.charAt(i + 1) match {
          case 'n' | 'N' => sb.append('\n')
          case x => sb.append(x)
        }
        i += 2
      } else { sb.append(c); i += 1 }
    }
    sb.toString
  }

  def escape(
    v: String
  ): String =
    v.replace("\\", "\\\\").replace(";", "\\;").replace(",", "\\,").replace("\r\n", "\\n").replace("\n", "\\n")

  private val Basic = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")

  /** DTSTART などの日時。TZID があればその地域の時刻、Z なら UTC、どちらもなければ zone の時刻とみなす。 */
  def time(
    p: IcalProp,
    zone: ZoneId
  ): Option[IcalTime] = {
    val v = p.value.trim
    if p.params.get("VALUE").exists(_.equalsIgnoreCase("DATE")) || v.length == 8 then
      Try(LocalDate.parse(v.take(8), DateTimeFormatter.BASIC_ISO_DATE)).toOption.map(IcalTime.Day(_))
    else
      Try(LocalDateTime.parse(v.stripSuffix("Z").take(15), Basic)).toOption.map { ldt =>
        val z = if v.endsWith("Z") then ZoneOffset.UTC
        else p.params.get("TZID").flatMap(t => Try(ZoneId.of(t)).toOption).getOrElse(zone)
        IcalTime.At(ldt.atZone(z).toInstant)
      }
  }

  /** DURATION（P1D・PT1H30M など）。 */
  def duration(
    v: String
  ): Option[Duration] =
    Try(Duration.parse(v.trim.replace("P0D", "PT0S"))).toOption.orElse {
      // java の Duration は日・時間だけ。週（P1W）は日にする
      "^P(\\d+)W$".r.findFirstMatchIn(v.trim).map(m => Duration.ofDays(m.group(1).toLong * 7))
    }

  private def person(
    p: IcalProp
  ): IcalPerson =
    IcalPerson(
      p.value.trim.replaceFirst("(?i)^mailto:", "").toLowerCase,
      p.params.get("CN").filter(_.nonEmpty),
      p.params.get("PARTSTAT").map(_.toUpperCase)
    )

  /** 招待を読む（最初の VEVENT）。zone は TZID のない時刻のタイムゾーン。 */
  def parse(
    text: String,
    zone: ZoneId
  ): Option[Invite] =
    if text.length > MaxBytes then None
    else
      Try {
        val lines = unfold(text).flatMap(prop)
        val method = lines.find(_.name == "METHOD").map(_.value.trim.toUpperCase).getOrElse("PUBLISH")
        val begin = lines.indexWhere(p => p.name == "BEGIN" && p.value.trim.equalsIgnoreCase("VEVENT"))
        if begin < 0 then None
        else {
          // VEVENT の中のプロパティ（入れ子の VALARM は除く）
          val ev = ListBuffer.empty[IcalProp]
          var depth = 0
          var i = begin + 1
          var done = false
          while i < lines.length && !done do {
            val p = lines(i)
            if p.name == "BEGIN" then depth += 1
            else if p.name == "END" then { if depth == 0 then done = true else depth -= 1 }
            else if depth == 0 then ev += p
            i += 1
          }
          def one(
            n: String
          ) = ev.find(_.name == n)
          for {
            uid <- one("UID").map(_.value.trim).filter(_.nonEmpty)
            start <- one("DTSTART").flatMap(time(_, zone))
          } yield {
            val end = one("DTEND").flatMap(time(_, zone)).orElse(one("DURATION").flatMap(d => duration(d.value)).map {
              d =>
                start match {
                  case IcalTime.Day(x) => IcalTime.Day(x.plusDays(math.max(1, d.toDays)))
                  case IcalTime.At(x) => IcalTime.At(x.plus(d))
                }
            }).getOrElse(start match {
              case IcalTime.Day(x) => IcalTime.Day(x.plusDays(1))
              case IcalTime.At(x) => IcalTime.At(x.plusSeconds(3600))
            })
            Invite(
              method,
              uid.take(500),
              one("SUMMARY").map(p => unescape(p.value)).getOrElse("").take(500),
              one("DESCRIPTION").map(p => unescape(p.value)).getOrElse("").take(10_000),
              one("LOCATION").map(p => unescape(p.value)).getOrElse("").take(500),
              start,
              end,
              one("ORGANIZER").map(person),
              ev.filter(_.name == "ATTENDEE").map(person).toList.take(500),
              one("SEQUENCE").flatMap(_.value.trim.toIntOption).getOrElse(0),
              one("RRULE").isDefined,
              ev.toList
            )
          }
        }
      }.toOption.flatten

  /** 75 オクテットで折り返す（UTF-8 の文字の途中では切らない）。 */
  def fold(
    line: String
  ): String = {
    val out = new StringBuilder
    var count = 0
    line.foreach { c =>
      val n = String.valueOf(c).getBytes(java.nio.charset.StandardCharsets.UTF_8).length
      if count + n > 75 then { out.append("\r\n "); count = 1 }
      out.append(c); count += n
    }
    out.toString
  }

  /** 招待への返事（METHOD:REPLY）。partstat: ACCEPTED・TENTATIVE・DECLINED。日時などは招待の行をそのまま使う。 */
  def reply(
    inv: Invite,
    me: String,
    myName: Option[String],
    partstat: String,
    now: Instant
  ): String = {
    val stamp = Basic.format(now.atZone(ZoneOffset.UTC)) + "Z"
    val copy =
      inv.props.filter(p => Set("DTSTART", "DTEND", "DURATION", "SUMMARY", "RECURRENCE-ID").contains(p.name)).map(_.raw)
    val cn = myName.filter(_.nonEmpty).map(n => s";CN=\"${n.replace("\"", "")}\"").getOrElse("")
    val lines = List(
      "BEGIN:VCALENDAR",
      "VERSION:2.0",
      "PRODID:-//skereru//skereru//JA",
      "METHOD:REPLY",
      "BEGIN:VEVENT",
      s"UID:${inv.uid}",
      s"DTSTAMP:$stamp",
      s"SEQUENCE:${inv.sequence}"
    ) ++ copy ++
      inv.organizer.map(o => s"ORGANIZER:mailto:${o.address}").toList ++
      List(s"ATTENDEE;PARTSTAT=${partstat.toUpperCase}$cn:mailto:$me", "END:VEVENT", "END:VCALENDAR")
    lines.map(fold).mkString("", "\r\n", "\r\n")
  }

  /** メッセージの中の招待（text/calendar の部品か、.ics の添付）。 */
  def find(
    raw: Array[Byte],
    root: MimePart,
    zone: ZoneId
  ): Option[Invite] =
    root.leaves().map(_._2).iterator.filter { p =>
      p.mimeType == "text/calendar" || p.mimeType == "application/ics" ||
      p.filename.exists(_.toLowerCase.endsWith(".ics"))
    }.flatMap(p => parse(Mime.text(raw, p), zone)).nextOption()
}
