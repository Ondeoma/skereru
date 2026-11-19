package skereru.core

import skereru.shared.*

/** 受信したメッセージに振り分けの規則を当てはめる。 */
object Rules {

  /** 規則の結果（移動先の名前・既読・スター・転送先・迷惑メールにしないか）。 */
  final case class Outcome(
    moveTo: Option[String],
    markRead: Boolean,
    flag: Boolean,
    trash: Boolean,
    forwards: List[String],
    notSpam: Boolean) {
    def merge(
      o: Outcome
    ): Outcome =
      Outcome(
        o.moveTo.orElse(moveTo),
        markRead || o.markRead,
        flag || o.flag,
        trash || o.trash,
        forwards ++ o.forwards,
        notSpam || o.notSpam
      )
  }

  val empty: Outcome = Outcome(None, false, false, false, Nil, false)

  /** 条件と一致するか（大文字小文字は区別しない）。 */
  def matches(
    r: FilterRuleDto,
    msg: Array[Byte],
    root: MimePart,
    envelopeTo: List[String]
  ): Boolean =
    if r.conditions.isEmpty then false
    else {
      lazy val body = root.leaves().map(_._2).filter(_.mainType == "text").take(2).map { p =>
        val t = Mime.text(msg, p)
        if p.subType == "html" then Html.toText(t) else t
      }.mkString("\n").take(200_000).toLowerCase
      def values(
        c: RuleConditionDto
      ): List[String] = c.field match {
        case RuleField.From =>
          root.header("From").toList.flatMap(v => Address.parseList(v).flatMap(a => List(a.display, a.address)))
        case RuleField.To =>
          root.header("To").toList.flatMap(v => Address.parseList(v).flatMap(a => List(a.display, a.address))) ++
            envelopeTo
        case RuleField.Cc =>
          root.header("Cc").toList.flatMap(v => Address.parseList(v).flatMap(a => List(a.display, a.address)))
        case RuleField.Recipient => (root.header("To").toList ++ root.header("Cc").toList).flatMap(v =>
            Address.parseList(v).flatMap(a => List(a.display, a.address))
          ) ++ envelopeTo
        case RuleField.Subject => root.headerDecoded("Subject").toList
        case RuleField.Body => List(body)
        case RuleField.ListId => root.headerDecoded("List-Id").toList
        case RuleField.Header =>
          c.header.toList.flatMap(h => root.headers.filter(_.name.equalsIgnoreCase(h.trim)).map(_.decoded))
        case _ => Nil
      }
      def test(
        c: RuleConditionDto
      ): Boolean = {
        val v = c.value.trim.toLowerCase
        val vs = values(c).map(_.toLowerCase)
        c.op match {
          case RuleOp.Contains => vs.exists(_.contains(v))
          case RuleOp.NotContains => !vs.exists(_.contains(v))
          case RuleOp.Equals => vs.exists(_.trim == v)
          case RuleOp.StartsWith => vs.exists(_.trim.startsWith(v))
          case RuleOp.EndsWith => vs.exists(_.trim.endsWith(v))
          case _ => false
        }
      }
      if r.matchAll then r.conditions.forall(test) else r.conditions.exists(test)
    }

  /** 有効な規則を順に当てはめる（「以降の規則を適用しない」で止まる）。 */
  def apply(
    rules: List[FilterRuleDto],
    msg: Array[Byte],
    root: MimePart,
    envelopeTo: List[String]
  ): Outcome =
    rules.filter(_.enabled).sortBy(_.position).foldLeft((empty, false)) { case ((acc, stopped), r) =>
      if stopped || !matches(r, msg, root, envelopeTo) then (acc, stopped)
      else {
        val o = r.actions.foldLeft(empty) {
          (
            o,
            a
          ) =>
            a.kind match {
              case RuleAction.Move => o.copy(moveTo = a.arg.map(_.trim).filter(_.nonEmpty))
              case RuleAction.MarkRead => o.copy(markRead = true)
              case RuleAction.Flag => o.copy(flag = true)
              case RuleAction.Delete => o.copy(trash = true)
              case RuleAction.Forward => o.copy(forwards = o.forwards ++ a.arg.map(_.trim).filter(Address.valid).toList)
              case RuleAction.NotSpam => o.copy(notSpam = true)
              case _ => o
            }
        }
        (acc.merge(o), r.actions.exists(_.kind == RuleAction.Stop))
      }
    }._1

  /** 規則の入力の確認。 */
  def validate(
    r: FilterRuleReq
  ): Either[String, FilterRuleReq] =
    if r.name.trim.isEmpty then Left("名前を入力してください")
    else if r.conditions.isEmpty then Left("条件を1つ以上指定してください")
    else if r.conditions.size > 20 || r.actions.size > 10 then Left("条件・処理が多すぎます")
    else if r.conditions.exists(c => !RuleField.all.contains(c.field) || !RuleOp.all.contains(c.op)) then
      Left("条件の種類が正しくありません")
    else if r.conditions.exists(c => c.value.trim.isEmpty && c.op != RuleOp.NotContains) then Left("条件の値を入力してください")
    else if r.conditions.exists(c => c.field == RuleField.Header && c.header.forall(_.trim.isEmpty)) then
      Left("ヘッダーの名前を入力してください")
    else if r.actions.isEmpty then Left("処理を1つ以上指定してください")
    else if r.actions.exists(a => !RuleAction.all.contains(a.kind)) then Left("処理の種類が正しくありません")
    else if r.actions.exists(a => RuleAction.needsArg(a.kind) && a.arg.forall(_.trim.isEmpty)) then
      Left("移動先・転送先を指定してください")
    else if r.actions.exists(a => a.kind == RuleAction.Forward && !a.arg.exists(x => Address.valid(x.trim))) then
      Left("転送先のメールアドレスが正しくありません")
    else Right(r.copy(name = r.name.trim.take(100)))
}
