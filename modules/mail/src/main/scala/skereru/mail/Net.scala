package skereru.mail

/** 行の長さの上限を超えた。 */
final class LineTooLong extends Exception("line too long")

/** 待ち受けの設定。 */
final case class Listener(name: String, port: Int, implicitTls: Boolean)
