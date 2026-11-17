package skereru.mail

import skereru.core.*
import skereru.db.*

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.Base64

/** SMTP の待ち受けの種類。Mx は外からの受信（25番）、Submission はメールソフトからの送信（587番・465番。認証が必須）。 */
enum SmtpMode:
  case Mx, Submission

/** DATA の本文を溜めるのに、全接続で使ってよいメモリの量（ヒープの 1/4）。
  * 1接続ごとの上限（maxMessageBytes）だけだと、終わりの "." を送らない接続を並べてメモリを使い切らせられるため。 */
object DataBudget:
  private val limit = Runtime.getRuntime.maxMemory() / 4
  private val used = new java.util.concurrent.atomic.AtomicLong(0)
  /** n バイトを借りる。足りなければ false。 */
  def acquire(n: Long): Boolean =
    val after = used.addAndGet(n)
    if after > limit then { used.addAndGet(-n); false } else true
  def release(n: Long): Unit = used.addAndGet(-n)

/** SMTP のセッション（RFC 5321）。 */
final class SmtpSession(conn: Conn, mode: SmtpMode, cfg: MailConfig, tls: Option[TlsHolder], b: Bridge, delivery: Delivery, auth: MailAuth,
    queue: QueueStore, log: String => Unit):

  private var helo: Option[String] = None
  private var user: Option[User]   = None
  private var mailFrom: Option[String] = None
  private val rcpts = scala.collection.mutable.ListBuffer.empty[String]
  private var errors = 0
  private var badRcpts = 0

  private def reply(s: String): Unit = conn.write(s + "\r\n")
  private def multi(code: String, lines: List[String]): Unit =
    conn.write(lines.init.map(l => s"$code-$l\r\n").mkString + s"$code ${lines.last}\r\n")

  private def err(s: String): Unit =
    errors += 1
    reply(s)
    if errors >= 10 then
      reply("421 4.7.0 Too many errors, closing connection")
      throw new java.io.EOFException()

  private def canAuth: Boolean = mode == SmtpMode.Submission && (conn.isTls || cfg.allowPlainAuth)

  def run(): Unit =
    conn.setTimeout(5 * 60 * 1000)
    if mode == SmtpMode.Mx then conn.deadline = System.currentTimeMillis() + Conn.MxSessionMs
    reply(s"220 ${cfg.hostname} ESMTP skereru")
    var open = true
    while open do
      conn.readLine(4096) match
        case None => open = false
        case Some(line) =>
          val (verb, arg) = line.indexOf(' ') match
            case -1 => (line.toUpperCase, "")
            case i  => (line.substring(0, i).toUpperCase, line.substring(i + 1))
          verb match
            case "EHLO" | "HELO" => hello(verb, arg.trim)
            case "STARTTLS" => starttls()
            case "AUTH" => authenticate(arg)
            case "MAIL" => mail(arg)
            case "RCPT" => rcpt(arg)
            case "DATA" => data()
            case "RSET" => reset(); reply("250 2.0.0 OK")
            case "NOOP" => reply("250 2.0.0 OK")
            case "VRFY" => reply("252 2.5.2 Cannot VRFY user")
            case "QUIT" => reply(s"221 2.0.0 ${cfg.hostname} closing connection"); open = false
            case "BDAT" => err("502 5.5.1 CHUNKING is not supported")
            case _ if line.startsWith("GET ") || line.startsWith("POST ") => reply("554 5.7.0 This is not an HTTP server"); open = false
            case _ => err("500 5.5.2 Command not recognized")

  private def reset(): Unit = { mailFrom = None; rcpts.clear() }

  private def hello(verb: String, name: String): Unit =
    if name.isEmpty then err("501 5.5.4 Syntax: EHLO hostname")
    else
      helo = Some(name.take(255))
      reset()
      if verb == "HELO" then reply(s"250 ${cfg.hostname}")
      else
        val ext = List(
          Some(s"${cfg.hostname} Hello"),
          Some("PIPELINING"),
          Some(s"SIZE ${cfg.maxMessageBytes}"),
          Some("8BITMIME"),
          Some("ENHANCEDSTATUSCODES"),
          Option.when(!conn.isTls && tls.isDefined)("STARTTLS"),
          Option.when(canAuth)("AUTH PLAIN LOGIN"),
          Some("SMTPUTF8")
        ).flatten
        multi("250", ext)

  private def starttls(): Unit =
    if conn.isTls then err("503 5.5.1 TLS already active")
    else tls match
      case None => err("454 4.7.0 TLS not available")
      case Some(t) =>
        // STARTTLS の後ろに続けて送られた平文のコマンドは捨てる（CVE-2011-0411 の対策）
        reply("220 2.0.0 Ready to start TLS")
        val ip = conn.clientIp
        conn.startTls(t.context)
        conn.clientIp = ip
        helo = None; user = None; reset()

  private def authenticate(arg: String): Unit =
    if !canAuth then err(if mode == SmtpMode.Mx then "503 5.5.1 AUTH not available on this port" else "538 5.7.11 Encryption required for requested authentication mechanism")
    else if user.isDefined then err("503 5.5.1 Already authenticated")
    else if helo.isEmpty then err("503 5.5.1 EHLO first")
    else
      val parts = arg.trim.split("\\s+").toList
      def ask(prompt: String): Option[String] =
        reply(s"334 $prompt")
        conn.readLine(2048).filterNot(_ == "*")
      def b64(s: String) = scala.util.Try(new String(Base64.getDecoder.decode(s.trim), StandardCharsets.UTF_8)).toOption
      val creds: Option[(String, String)] = parts.headOption.map(_.toUpperCase) match
        case Some("PLAIN") =>
          val resp = parts.lift(1).orElse(ask(""))
          resp.flatMap(b64).flatMap { s =>
            s.split('\u0000') match
              case Array(_, u, p) => Some((u, p))
              case _              => None
          }
        case Some("LOGIN") =>
          for
            u <- parts.lift(1).orElse(ask(Base64.getEncoder.encodeToString("Username:".getBytes))).flatMap(b64)
            p <- ask(Base64.getEncoder.encodeToString("Password:".getBytes)).flatMap(b64)
          yield (u, p)
        case _ => { err("504 5.5.4 Unrecognized authentication type"); return }
      creds match
        case None => err("501 5.5.2 Cannot decode response")
        case Some((u, p)) =>
          b.run(auth.login(u, p, conn.clientIp, "smtp")) match
            case Right(usr) => user = Some(usr); conn.deadline = Long.MaxValue; reply("235 2.7.0 Authentication successful")
            case Left(_)    => err("535 5.7.8 Authentication credentials invalid")

  /** "FROM:<a@b> SIZE=123" の形を読む。 */
  private def pathArg(arg: String, prefix: String): Option[(String, Map[String, String])] =
    val a = arg.trim
    if !a.toUpperCase.startsWith(prefix) then None
    else
      val rest = a.substring(prefix.length).trim
      if !rest.startsWith("<") then None
      else
        val end = rest.indexOf('>')
        if end < 0 then None
        else
          val path = rest.substring(1, end).trim
          // ソースルート（@a,@b:user@host）は捨てる
          val addr = if path.startsWith("@") && path.contains(':') then path.substring(path.indexOf(':') + 1) else path
          val params = rest.substring(end + 1).trim.split("\\s+").toList.filter(_.nonEmpty).map { p =>
            p.indexOf('=') match
              case -1 => p.toUpperCase -> ""
              case i  => p.substring(0, i).toUpperCase -> p.substring(i + 1)
          }.toMap
          Some((addr, params))

  private def mail(arg: String): Unit =
    if helo.isEmpty then err("503 5.5.1 EHLO/HELO first")
    else if mailFrom.isDefined then err("503 5.5.1 Nested MAIL command")
    else if mode == SmtpMode.Submission && user.isEmpty then err("530 5.7.0 Authentication required")
    else pathArg(arg, "FROM:") match
      case None => err("501 5.5.4 Syntax: MAIL FROM:<address>")
      case Some((addr, params)) =>
        val size = params.get("SIZE").flatMap(_.toLongOption)
        if addr.nonEmpty && !Address.valid(Address.toAscii(addr)) then err("553 5.1.7 Bad sender address syntax")
        else if size.exists(_ > cfg.maxMessageBytes) then err("552 5.3.4 Message size exceeds fixed maximum message size")
        else if params.get("BODY").exists(b => !Set("7BIT", "8BITMIME").contains(b.toUpperCase)) then err("555 5.5.4 Unsupported BODY parameter")
        else if params.keySet.exists(k => !Set("SIZE", "BODY", "SMTPUTF8").contains(k)) then err("555 5.5.4 Unsupported parameter")
        else
          mailFrom = Some(Address.toAscii(addr))
          reply("250 2.1.0 OK")

  private def rcpt(arg: String): Unit =
    if mailFrom.isEmpty then err("503 5.5.1 MAIL first")
    else if rcpts.size >= cfg.maxRecipients then reply("452 4.5.3 Too many recipients")
    else pathArg(arg, "TO:") match
      case None => err("501 5.5.4 Syntax: RCPT TO:<address>")
      case Some((addr0, _)) =>
        val addr = Address.toAscii(addr0).toLowerCase
        if addr.equalsIgnoreCase("postmaster") then rcpt(s"TO:<postmaster@${cfg.hostname}>")
        else if !Address.valid(addr) then err("553 5.1.3 Bad recipient address syntax")
        else
          val local = b.run(delivery.isLocalDomain(Address.domainOf(addr)))
          if mode == SmtpMode.Mx then
            if !local then err("554 5.7.1 Relay access denied")
            else b.run(delivery.resolve(addr)) match
              case None =>
                badRcpts += 1
                // 宛先の探索（辞書攻撃）を遅らせて、続けば切る
                Thread.sleep(math.min(5000L, 500L * badRcpts))
                if badRcpts >= 5 then { reply("421 4.7.0 Too many invalid recipients"); throw new java.io.EOFException() }
                reply("550 5.1.1 Recipient address rejected: User unknown")
              case Some(_) =>
                if cfg.greylist && !greylisted(addr) then reply("451 4.7.1 Greylisted, please try again later")
                else { rcpts += addr; reply("250 2.1.5 OK") }
          else
            rcpts += addr
            reply("250 2.1.5 OK")

  /** グレイリスト（送信元の /24・差出人・宛先の組）。通ってよければ true。 */
  private def greylisted(rcpt: String): Boolean =
    val ip = conn.clientIp match
      case a: java.net.Inet4Address => a.getAddress.take(3).map(_ & 0xff).mkString(".")
      case a => a.getAddress.take(8).map(x => f"${x & 0xff}%02x").mkString
    b.run(queue.greylist(s"$ip|${mailFrom.getOrElse("").toLowerCase}|$rcpt", cfg.greylistDelaySecs))

  private def data(): Unit =
    if mailFrom.isEmpty then err("503 5.5.1 MAIL first")
    else if rcpts.isEmpty then err("554 5.5.1 No valid recipients")
    else
      reply("354 End data with <CR><LF>.<CR><LF>")
      // 借りたメモリは、保存（finish）が終わってから返す
      var borrowed = 0L
      try
        readData(n => { val ok = DataBudget.acquire(n); if ok then borrowed += n; ok }) match
          case Left(e) => reply(e)
          case Right(raw) =>
            // 保存の途中の失敗（DB の切り替えなど）は一時的なエラーにして、相手に再送してもらう
            try finish(raw)
            catch
              case e: java.io.IOException => throw e
              case e: Throwable =>
                log(s"[smtp] ${conn.clientIp.getHostAddress}: $e")
                reply("451 4.3.0 Temporary failure, please try again later")
      finally DataBudget.release(borrowed)
      reset()

  /** DATA の本文を読む（ドットの戻し、大きさの上限、SMTP スマグリングの対策）。
    * 単独の LF で終わる行は CRLF に直して受け付ける（送り方の雑な送信元がある）が、データの終わりの "." の行が
    * 単独の LF・CR と隣り合うもの（"\n.\r\n"・"\r\n.\n" など。別のメッセージを紛れ込ませる手口）と、行の途中の単独の CR は拒否する。 */
  /** DATA の全体にかけてよい時間。1回の読み込みごとのタイムアウトだけだと、少しずつ送って接続を保てるため。 */
  private val DataDeadlineMs = 10 * 60 * 1000L

  private def readData(borrow: Long => Boolean): Either[String, Array[Byte]] =
    val out = new ByteArrayOutputStream(64 * 1024)
    val deadline = System.currentTimeMillis() + DataDeadlineMs
    var reserved = 0L
    var busy = false
    var tooBig = false
    var smuggle = false
    var prevBare = false
    var done = false
    while !done do
      conn.readRawLine(64 * 1024) match
        case None => throw new java.io.EOFException()
        case Some(line) =>
          val crlf = line.length >= 2 && line(line.length - 2) == '\r' && line(line.length - 1) == '\n'
          val lf = !crlf && line.nonEmpty && line.last == '\n'
          val body = if crlf then line.dropRight(2) else if lf then line.dropRight(1) else line
          val isDot = body.length == 1 && body(0) == '.'
          if crlf && isDot then
            if prevBare then smuggle = true
            done = true
          else
            if isDot || body.contains('\r'.toByte) || (!crlf && !lf) then smuggle = true
            if !tooBig && !busy then
              val unstuffed = if body.nonEmpty && body(0) == '.' then body.drop(1) else body
              // 溜める量の3倍（後でコピーが重なる分）を、1MB ずつ先に借りる
              while !busy && (out.size() + unstuffed.length + 2) * 3L > reserved do
                if borrow(1024 * 1024) then reserved += 1024 * 1024 else { busy = true; out.reset() }
              if !busy then
                out.write(unstuffed)
                out.write('\r'); out.write('\n')
                if out.size() > cfg.maxMessageBytes then { tooBig = true; out.reset() }
          prevBare = !crlf
          if !done && System.currentTimeMillis() > deadline then
            reply("451 4.4.2 Timeout during DATA")
            throw new java.io.EOFException()
    if tooBig then Left("552 5.3.4 Message size exceeds fixed maximum message size")
    else if busy then Left("452 4.3.1 Insufficient system resources, please try again later")
    else if smuggle then Left("550 5.6.11 Bare LF or CR next to the end of data is not allowed (use CRLF)")
    else Right(out.toByteArray)

  private def finish(raw: Array[Byte]): Unit =
    val env = Envelope(conn.clientIp, helo.getOrElse(""), mailFrom.getOrElse(""), rcpts.toList, conn.isTls, user.map(_.id))
    // 1通ごとの受け取り（inbound・submit）は Delivery がスパンにする（このセッションのスパンの子）
    mode match
      case SmtpMode.Mx =>
        b.run(delivery.inbound(env, raw)) match
          case InboundResult.Accepted(id)       => reply(s"250 2.0.0 OK queued as $id")
          case InboundResult.Rejected(code, m)  => reply(s"${code.take(3)} ${code.drop(4)} $m")
          case InboundResult.TempFail(code, m)  => reply(s"${code.take(3)} ${code.drop(4)} $m")
      case SmtpMode.Submission =>
        b.run(delivery.submit(user.get, env.mailFrom, env.rcpts, raw, if conn.isTls then "ESMTPSA" else "ESMTPA", Some(conn.clientIp.getHostAddress))) match
          case Right(_) => reply("250 2.0.0 OK queued")
          case Left(e)  => reply(s"550 5.7.1 ${e.replaceAll("[\r\n]", " ")}")
