package skereru.mail

import skereru.core.*
import skereru.db.*
import skereru.shared.{Flag, SpecialUse}

import java.nio.charset.StandardCharsets
import java.util.Base64

/** POP3 のセッション（RFC 1939 + STLS・UIDL・TOP・SASL PLAIN）。受信トレイだけを扱う。 */
final class Pop3Session(conn: Conn,
                        cfg: MailConfig,
                        tls: Option[TlsHolder],
                        b: Bridge,
                        mail: MailStore,
                        blobs: skereru.storage.Blobs,
                        auth: MailAuth) {

  private final case class Item(uid: Long, size: Int, blobId: String)

  private var user: Option[User] = None
  private var box: Option[Mailbox] = None
  private var items = Vector.empty[Item]
  private val deleted = scala.collection.mutable.Set.empty[Int]
  private var pendingUser: Option[String] = None
  private var errors = 0

  private def ok(s: String = ""): Unit = conn.write(s"+OK $s\r\n")
  private def fail(s: String): Unit = {
    errors += 1
    conn.write(s"-ERR $s\r\n")
    if errors >= 10 then throw new java.io.EOFException()
  }

  private def canAuth = conn.isTls || cfg.allowPlainAuth

  def run(): Unit = {
    conn.setTimeout(10 * 60 * 1000)
    ok(s"${cfg.hostname} POP3 skereru ready")
    var open = true
    while open do
      conn.readLine(1024) match {
        case None => open = false
        case Some(line) =>
          val parts = line.trim.split(" ", 2)
          val cmd = parts(0).toUpperCase
          val arg = parts.lift(1).getOrElse("").trim
          (cmd, user) match {
            case ("CAPA", _) =>
              val caps = List("TOP", "UIDL", "USER", "RESP-CODES", "AUTH-RESP-CODE", "PIPELINING") ++
                (if !conn.isTls && tls.isDefined then List("STLS") else Nil) ++
                (if canAuth then List("SASL PLAIN") else Nil)
              conn.write("+OK Capability list follows\r\n" + caps.map(_ + "\r\n").mkString + ".\r\n")
            case ("QUIT", _) => quit(); open = false
            case ("NOOP", Some(_)) => ok()
            case ("STLS", None) =>
              tls match {
                case Some(t) if !conn.isTls =>
                  ok("Begin TLS negotiation")
                  val ip = conn.clientIp
                  conn.startTls(t.context)
                  conn.clientIp = ip
                case _ => fail("TLS not available")
              }
            case ("USER", None) =>
              if !canAuth then fail("[SYS/PERM] Use STLS first") else { pendingUser = Some(arg); ok() }
            case ("PASS", None) =>
              pendingUser match {
                case None => fail("USER first")
                case Some(u) => login(u, arg)
              }
            case ("AUTH", None) =>
              if !canAuth then fail("[SYS/PERM] Use STLS first")
              else {
                val p = arg.split(" ").toList
                if !p.headOption.exists(_.equalsIgnoreCase("PLAIN")) then fail("Unsupported mechanism")
                else {
                  val resp = p.lift(1).orElse { conn.write("+ \r\n"); conn.readLine(4096) }
                  resp.flatMap(r =>
                    scala.util.Try(new String(Base64.getDecoder.decode(r.trim), StandardCharsets.UTF_8)).toOption
                  ).map(_.split('\u0000')) match {
                    case Some(Array(_, u, pw)) => login(u, pw)
                    case _ => fail("Invalid response")
                  }
                }
              }
            case ("STAT", Some(_)) =>
              val live = visible
              ok(s"${live.size} ${live.map(_._2.size).sum}")
            case ("LIST", Some(_)) => listing(
                arg,
                (_, i) => i.size.toString
              )
            case ("UIDL", Some(_)) => listing(
                arg,
                (_, i) => s"${box.get.uidvalidity}-${i.uid}"
              )
            case ("RETR", Some(_)) => msg(arg).foreach((_, i) => send(i, None))
            case ("TOP", Some(_)) =>
              val p = arg.split(" ")
              p.lift(1).flatMap(_.toIntOption).filter(_ >= 0) match {
                case None => fail("Syntax: TOP msg n")
                case Some(n) => msg(p(0)).foreach((_, i) => send(i, Some(n)))
              }
            case ("DELE", Some(_)) => msg(arg).foreach {
                (n, _) => deleted += n; ok(s"message $n deleted")
              }
            case ("RSET", Some(_)) => deleted.clear(); ok()
            case (_, None) => fail("Not authenticated")
            case _ => fail("Unknown command")
          }
      }
  }

  private def login(u: String, p: String): Unit =
    b.run(auth.login(u, p, conn.clientIp, "pop3")) match {
      case Left(_) => fail("[AUTH] Authentication failed")
      case Right(usr) =>
        conn.deadline = Long.MaxValue
        b.run(mail.ensureDefaults(usr.id))
        val inbox = b.run(mail.special(usr.id, SpecialUse.Inbox)).get
        val rows = b.run(mail.uidFlags(inbox.id)).filterNot(_.flags.contains(Flag.Deleted))
        val metas = rows.map(_.uid).grouped(1000).flatMap(g => b.run(mail.byUids(inbox.id, g))).toVector
        user = Some(usr); box = Some(inbox)
        items = metas.map(m => Item(m.uid, m.size, m.blobId))
        ok(s"Logged in, ${items.size} messages")
    }

  private def visible: Vector[(Int, Item)] =
    items.zipWithIndex.map((i, k) => (k + 1, i)).filterNot((n, _) => deleted.contains(n))

  private def msg(arg: String): Option[(Int, Item)] =
    arg.trim.toIntOption.flatMap(n => visible.find(_._1 == n)) match {
      case None => fail("No such message"); None
      case some => some
    }

  private def listing(arg: String, f: (Int, Item) => String): Unit =
    if arg.nonEmpty then
      msg(arg).foreach((n, i) => ok(s"$n ${f(n, i)}"))
    else
      conn.write("+OK\r\n" + visible.map((n, i) => s"$n ${f(n, i)}\r\n").mkString + ".\r\n")

  /** メッセージを送る（行頭の . は .. にする）。lines があれば本文の先頭 n 行まで。 */
  private def send(i: Item, lines: Option[Int]): Unit = {
    val raw = b.run(blobs.get(i.blobId)).getOrElse(Array.emptyByteArray)
    val text = new String(raw, StandardCharsets.ISO_8859_1)
    val body = lines match {
      case None => text
      case Some(n) =>
        val he = Mime.headerEnd(raw, 0, raw.length)
        text.substring(0, he) + text.substring(he).split("\r\n", -1).take(n).mkString("\r\n")
    }
    val stuffed = body.split("\r\n", -1).map(l => if l.startsWith(".") then "." + l else l).mkString("\r\n")
    conn.writeRaw("+OK message follows\r\n" + stuffed + (if stuffed.endsWith("\r\n") then "" else "\r\n") + ".\r\n")
    conn.flush()
  }

  /** QUIT（UPDATE 状態: 削除の印を付けたものを消す）。 */
  private def quit(): Unit = {
    (box, user) match {
      case (Some(bx), Some(_)) if deleted.nonEmpty =>
        val uids = deleted.toList.flatMap(n => items.lift(n - 1)).map(_.uid)
        b.run(mail.storeFlags(bx.id, uids, "add", List(Flag.Deleted)))
        b.run(mail.expunge(bx.id, Some(uids)))
      case _ => ()
    }
    ok(s"${cfg.hostname} closing connection")
  }
}
