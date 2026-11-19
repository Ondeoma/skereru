package skereru.core

/**
 * 画面・API の検索の構文（Gmail に近い）。
 * from:a to:b subject:c has:attachment is:unread is:read is:starred is:snoozed in:受信トレイ before:2024-01-01 after:2024-01-01 larger:1M smaller:100K と自由な語。
 */
final case class MailQuery(
  words: List[String],
  from: List[String],
  to: List[String],
  subject: List[String],
  hasAttachment: Option[Boolean],
  unread: Option[Boolean],
  starred: Option[Boolean],
  in: Option[String],
  before: Option[java.time.LocalDate],
  after: Option[java.time.LocalDate],
  larger: Option[Long],
  smaller: Option[Long],
  /** is:snoozed（スヌーズ中のものだけ）。指定がなければスヌーズ中のものは出さない。 */
  snoozed: Option[Boolean] = None)

object MailQuery {
  private def size(s: String): Option[Long] = {
    val t = s.trim.toUpperCase
    val (num, mul) = t.lastOption match {
      case Some('K') => (t.dropRight(1), 1024L)
      case Some('M') => (t.dropRight(1), 1024L * 1024)
      case Some('G') => (t.dropRight(1), 1024L * 1024 * 1024)
      case _ => (t, 1L)
    }
    num.toDoubleOption.map(d => (d * mul).toLong)
  }

  /** 語に分ける（"…" で囲めば空白を含められる）。 */
  def tokens(q: String): List[String] =
    """(\S+:"[^"]*"|"[^"]*"|\S+)""".r.findAllIn(q).toList.map(_.replace("\"", ""))

  def parse(q: String): MailQuery = {
    val empty = MailQuery(Nil, Nil, Nil, Nil, None, None, None, None, None, None, None, None)
    tokens(q).foldLeft(empty) {
      (m, t) =>
        {
          val i = t.indexOf(':')
          val (k, v) = if i > 0 then (t.substring(0, i).toLowerCase, t.substring(i + 1)) else ("", t)
          def date = scala.util.Try(java.time.LocalDate.parse(v.replace('/', '-'))).toOption
          k match {
            case "from" if v.nonEmpty => m.copy(from = m.from :+ v)
            case "to" if v.nonEmpty => m.copy(to = m.to :+ v)
            case "subject" if v.nonEmpty => m.copy(subject = m.subject :+ v)
            case "has" if v.equalsIgnoreCase("attachment") => m.copy(hasAttachment = Some(true))
            case "is" => v.toLowerCase match {
                case "unread" => m.copy(unread = Some(true))
                case "read" => m.copy(unread = Some(false))
                case "starred" => m.copy(starred = Some(true))
                case "snoozed" => m.copy(snoozed = Some(true))
                case _ => m.copy(words = m.words :+ t)
              }
            case "in" if v.nonEmpty => m.copy(in = Some(v))
            case "before" if date.isDefined => m.copy(before = date)
            case "after" if date.isDefined => m.copy(after = date)
            case "larger" if size(v).isDefined => m.copy(larger = size(v))
            case "smaller" if size(v).isDefined => m.copy(smaller = size(v))
            case _ => if t.nonEmpty then m.copy(words = m.words :+ t) else m
          }
        }
    }
  }
}
