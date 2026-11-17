package skereru.core

import skereru.shared.*

/** 受信したメッセージに振り分けの規則を当てはめる。 */
object Rules:

  /** 規則の結果（移動先の名前・既読・スター・転送先・迷惑メールにしないか）。 */
  final case class Outcome(moveTo: Option[String], markRead: Boolean, flag: Boolean, trash: Boolean, forwards: List[String], notSpam: Boolean):
    def merge(o: Outcome): Outcome =
      Outcome(o.moveTo.orElse(moveTo), markRead || o.markRead, flag || o.flag, trash || o.trash, forwards ++ o.forwards, notSpam || o.notSpam)

  val empty: Outcome = Outcome(None, false, false, false, Nil, false)

  /** 条件と一致するか（大文字小文字は区別しない）。 */
  def matches(r: FilterRuleDto, msg: Array[Byte], root: MimePart, envelopeTo: List[String]): Boolean =
    if r.conditions.isEmpty then false
    else
      lazy val body = root.leaves().map(_._2).filter(_.mainType == "text").take(2).map { p =>
        val t = Mime.text(msg, p)
        if p.subType == "html" then Html.toText(t) else t
      }.mkString("\n").take(200_000).toLowerCase
      def values(c: RuleConditionDto): List[String] = c.field match
        case RuleField.From      => root.header("From").toList.flatMap(v => Address.parseList(v).flatMap(a => List(a.display, a.address)))
        case RuleField.To        => root.header("To").toList.flatMap(v => Address.parseList(v).flatMap(a => List(a.display, a.address))) ++ envelopeTo
        case RuleField.Cc        => root.header("Cc").toList.flatMap(v => Address.parseList(v).flatMap(a => List(a.display, a.address)))
        case RuleField.Recipient => (root.header("To").toList ++ root.header("Cc").toList).flatMap(v => Address.parseList(v).flatMap(a => List(a.display, a.address))) ++ envelopeTo
        case RuleField.Subject   => root.headerDecoded("Subject").toList
        case RuleField.Body      => List(body)
        case RuleField.ListId    => root.headerDecoded("List-Id").toList
        case RuleField.Header    => c.header.toList.flatMap(h => root.headers.filter(_.name.equalsIgnoreCase(h.trim)).map(_.decoded))
        case _                   => Nil
      def test(c: RuleConditionDto): Boolean =
        val v = c.value.trim.toLowerCase
        val vs = values(c).map(_.toLowerCase)
        c.op match
          case RuleOp.Contains    => vs.exists(_.contains(v))
          case RuleOp.NotContains => !vs.exists(_.contains(v))
          case RuleOp.Equals      => vs.exists(_.trim == v)
          case RuleOp.StartsWith  => vs.exists(_.trim.startsWith(v))
          case RuleOp.EndsWith    => vs.exists(_.trim.endsWith(v))
          case _                  => false
      if r.matchAll then r.conditions.forall(test) else r.conditions.exists(test)

  /** 有効な規則を順に当てはめる（「以降の規則を適用しない」で止まる）。 */
  def apply(rules: List[FilterRuleDto], msg: Array[Byte], root: MimePart, envelopeTo: List[String]): Outcome =
    rules.filter(_.enabled).sortBy(_.position).foldLeft((empty, false)) { case ((acc, stopped), r) =>
      if stopped || !matches(r, msg, root, envelopeTo) then (acc, stopped)
      else
        val o = r.actions.foldLeft(empty) { (o, a) =>
          a.kind match
            case RuleAction.Move     => o.copy(moveTo = a.arg.map(_.trim).filter(_.nonEmpty))
            case RuleAction.MarkRead => o.copy(markRead = true)
            case RuleAction.Flag     => o.copy(flag = true)
            case RuleAction.Delete   => o.copy(trash = true)
            case RuleAction.Forward  => o.copy(forwards = o.forwards ++ a.arg.map(_.trim).filter(Address.valid).toList)
            case RuleAction.NotSpam  => o.copy(notSpam = true)
            case _                   => o
        }
        (acc.merge(o), r.actions.exists(_.kind == RuleAction.Stop))
    }._1

  /** 規則の入力の確認。 */
  def validate(r: FilterRuleReq): Either[String, FilterRuleReq] =
    if r.name.trim.isEmpty then Left("名前を入力してください")
    else if r.conditions.isEmpty then Left("条件を1つ以上指定してください")
    else if r.conditions.size > 20 || r.actions.size > 10 then Left("条件・処理が多すぎます")
    else if r.conditions.exists(c => !RuleField.all.contains(c.field) || !RuleOp.all.contains(c.op)) then Left("条件の種類が正しくありません")
    else if r.conditions.exists(c => c.value.trim.isEmpty && c.op != RuleOp.NotContains) then Left("条件の値を入力してください")
    else if r.conditions.exists(c => c.field == RuleField.Header && c.header.forall(_.trim.isEmpty)) then Left("ヘッダーの名前を入力してください")
    else if r.actions.isEmpty then Left("処理を1つ以上指定してください")
    else if r.actions.exists(a => !RuleAction.all.contains(a.kind)) then Left("処理の種類が正しくありません")
    else if r.actions.exists(a => RuleAction.needsArg(a.kind) && a.arg.forall(_.trim.isEmpty)) then Left("移動先・転送先を指定してください")
    else if r.actions.exists(a => a.kind == RuleAction.Forward && !a.arg.exists(x => Address.valid(x.trim))) then Left("転送先のメールアドレスが正しくありません")
    else Right(r.copy(name = r.name.trim.take(100)))

/** 画面・API の検索の構文（Gmail に近い）。
  * from:a to:b subject:c has:attachment is:unread is:read is:starred is:snoozed in:受信トレイ before:2024-01-01 after:2024-01-01 larger:1M smaller:100K と自由な語。 */
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
    snoozed: Option[Boolean] = None
)

object MailQuery:
  private def size(s: String): Option[Long] =
    val t = s.trim.toUpperCase
    val (num, mul) = t.lastOption match
      case Some('K') => (t.dropRight(1), 1024L)
      case Some('M') => (t.dropRight(1), 1024L * 1024)
      case Some('G') => (t.dropRight(1), 1024L * 1024 * 1024)
      case _         => (t, 1L)
    num.toDoubleOption.map(d => (d * mul).toLong)

  /** 語に分ける（"…" で囲めば空白を含められる）。 */
  def tokens(q: String): List[String] =
    """(\S+:"[^"]*"|"[^"]*"|\S+)""".r.findAllIn(q).toList.map(_.replace("\"", ""))

  def parse(q: String): MailQuery =
    val empty = MailQuery(Nil, Nil, Nil, Nil, None, None, None, None, None, None, None, None)
    tokens(q).foldLeft(empty) { (m, t) =>
      val i = t.indexOf(':')
      val (k, v) = if i > 0 then (t.substring(0, i).toLowerCase, t.substring(i + 1)) else ("", t)
      def date = scala.util.Try(java.time.LocalDate.parse(v.replace('/', '-'))).toOption
      k match
        case "from"    if v.nonEmpty => m.copy(from = m.from :+ v)
        case "to"      if v.nonEmpty => m.copy(to = m.to :+ v)
        case "subject" if v.nonEmpty => m.copy(subject = m.subject :+ v)
        case "has" if v.equalsIgnoreCase("attachment") => m.copy(hasAttachment = Some(true))
        case "is" => v.toLowerCase match
            case "unread"  => m.copy(unread = Some(true))
            case "read"    => m.copy(unread = Some(false))
            case "starred" => m.copy(starred = Some(true))
            case "snoozed" => m.copy(snoozed = Some(true))
            case _         => m.copy(words = m.words :+ t)
        case "in" if v.nonEmpty => m.copy(in = Some(v))
        case "before" if date.isDefined => m.copy(before = date)
        case "after"  if date.isDefined => m.copy(after = date)
        case "larger"  if size(v).isDefined => m.copy(larger = size(v))
        case "smaller" if size(v).isDefined => m.copy(smaller = size(v))
        case _ => if t.nonEmpty then m.copy(words = m.words :+ t) else m
    }
