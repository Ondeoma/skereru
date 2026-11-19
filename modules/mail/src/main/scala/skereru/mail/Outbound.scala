package skereru.mail

/** 1つの宛先の結果。 */
enum RcptResult {
  case Ok
  case Temp(
    msg: String)
  case Perm(
    msg: String)
}

final case class SmtpReplyError(code: Int, text: String) extends Exception(text)
