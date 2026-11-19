package skereru.core

import java.time.LocalDate

/** IMAP の構文エラー（BAD で返す）。 */
final class ImapSyntax(msg: String) extends Exception(msg)

/** BODY[…] の部品の指定。 */
final case class Section(path: List[Int], kind: String, fields: List[String]) {
  def render: String = {
    val p = path.mkString(".")
    val k = kind match {
      case "" => ""
      case "HEADER.FIELDS" | "HEADER.FIELDS.NOT" => s"$kind (${fields.mkString(" ")})"
      case other => other
    }
    if p.nonEmpty && k.nonEmpty then s"$p.$k" else p + k
  }
}

enum FetchAtt {
  case Flags, Uid, InternalDate, Rfc822Size, Envelope, BodyStructure, Body, Rfc822, Rfc822Header, Rfc822Text, ModSeq
  case BodySection(
    peek: Boolean,
    section: Section,
    partial: Option[(Long, Long)])

  /** \Seen を付ける取得か（BODY[] や RFC822 など、PEEK でないもの）。 */
  def setsSeen: Boolean = this match {
    case Rfc822 | Rfc822Text => true
    case BodySection(peek, _, _) => !peek
    case _ => false
  }
}

enum SearchKey {
  case All
  case Flag(
    flag: String,
    set: Boolean
  ) // \Seen なら set=true、UNSEEN なら false
  case Keyword(
    k: String,
    set: Boolean)
  case Recent, New, Old
  case HeaderContains(
    name: String,
    value: String)
  case Body(
    text: String)
  case Text(
    text: String)
  case Before(
    d: LocalDate)
  case On(
    d: LocalDate)
  case Since(
    d: LocalDate)
  case SentBefore(
    d: LocalDate)
  case SentOn(
    d: LocalDate)
  case SentSince(
    d: LocalDate)
  case Larger(
    n: Long)
  case Smaller(
    n: Long)
  case Uid(
    set: SeqSet)
  case Seq(
    set: SeqSet)
  case Not(
    k: SearchKey)
  case Or(
    a: SearchKey,
    b: SearchKey)
  case And(
    keys: List[SearchKey])
  case ModSeqAbove(
    n: Long)
}
