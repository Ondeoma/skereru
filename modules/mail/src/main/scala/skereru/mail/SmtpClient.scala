package skereru.mail

import skereru.core.*

import java.nio.charset.StandardCharsets
import java.util.Base64

/** 外への SMTP の送信（MX を引いて、順に試す。STARTTLS は相手が対応していれば使う）。 */
final class SmtpClient(cfg: MailConfig, dns: JndiDns) {

  private def hosts(domain: String): Either[RcptResult, List[(String, Int)]] =
    cfg.relay match {
      case Some(r) => Right(List(r.host -> r.port))
      case None =>
        dns.mx(domain) match {
          case Left(e) => Left(RcptResult.Temp(s"DNS の問い合わせに失敗しました: $e"))
          case Right(Nil) =>
            // MX がなければ A/AAAA のホストへ（RFC 5321 5.1）
            dns.addresses(domain) match {
              case Right(Nil) => Left(RcptResult.Perm(s"550 5.1.2 ドメイン $domain にメールサーバーがありません"))
              case Right(_) => Right(List(domain -> 25))
              case Left(e) => Left(RcptResult.Temp(s"DNS の問い合わせに失敗しました: $e"))
            }
          case Right(List((0, ""))) | Right(List((0, "."))) =>
            Left(RcptResult.Perm(s"556 5.1.10 ドメイン $domain はメールを受け取りません（null MX）"))
          case Right(mxs) => Right(mxs.map(_._2 -> 25).take(5))
        }
    }

  /** インターネットの外（内部のネットワーク）を指すアドレスか。 */

  /** 送る。宛先ごとの結果を返す（同じドメインの宛先をまとめて渡す）。 */
  def deliver(domain: String, sender: String, rcpts: List[String], raw: Array[Byte]): Map[String, RcptResult] =
    hosts(domain) match {
      case Left(r) => rcpts.map(_ -> r).toMap
      case Right(hs) =>
        var last: Map[String, RcptResult] = rcpts.map(_ -> RcptResult.Temp("接続できるメールサーバーがありません")).toMap
        val it = hs.iterator
        var done = false
        while !done && it.hasNext do {
          val (h, p) = it.next()
          val r = try attempt(h, p, sender, rcpts, raw, tryTls = true)
          catch {
            // 証明書を確かめる設定のときは平文に戻さない（攻撃者が TLS を失敗させるだけで平文にできてしまうため）
            case _: javax.net.ssl.SSLException
                if !cfg.outboundRequireTls && !cfg.outboundVerifyTls && cfg.relay.isEmpty =>
              // TLS の交渉に失敗した相手には平文で送り直す（日和見的な暗号化）
              try attempt(h, p, sender, rcpts, raw, tryTls = false)
              catch case e2: Throwable => rcpts.map(_ -> RcptResult.Temp(s"$h: ${e2.getMessage}")).toMap
            case e: Throwable =>
              rcpts.map(_ -> RcptResult.Temp(s"$h: ${Option(e.getMessage).getOrElse(e.toString)}")).toMap
          }
          last = r
          // すべて一時的な失敗なら次の MX を試す
          done = !r.values.forall(_.isInstanceOf[RcptResult.Temp])
        }
        last
    }

  private def attempt(host: String,
                      port: Int,
                      sender: String,
                      rcpts: List[String],
                      raw: Array[Byte],
                      tryTls: Boolean): Map[String, RcptResult] = {
    val relay = cfg.relay
    val implicitTls = relay.exists(_.tls == "ssl")
    // MX の名前が内部のアドレスを指していたら接続しない（中継サーバーは管理者が設定したものなので対象外）
    // 確かめた IP にそのままつなぐ（名前でつなぐと、OS がもう一度名前を引き、その間に内部のアドレスに変えられる＝DNS リバインディング）
    val target: Option[java.net.InetAddress] =
      if relay.isEmpty && !cfg.outboundAllowPrivate then {
        sbirds.net.NetGuard.pick(host, dns.addresses(host).toOption.getOrElse(Nil)) match {
          case Left(_) =>
            return rcpts.map(_ -> RcptResult.Perm(s"550 5.4.4 $host は内部のアドレスを指しているため送れません")).toMap
          case Right(ip) => Some(ip)
        }
      } else None
    // TLS の名前（SNI・証明書の確認）には、IP ではなくホスト名を使う（startClientTls に host を渡している）
    val c = target.fold(Conn.client(host, port, 30_000))(ip => Conn.client(ip, port, 30_000))
    try {
      if implicitTls then c.startClientTls(host, verify = true)
      def read(): (Int, String) = {
        val lines = scala.collection.mutable.ListBuffer.empty[String]
        var more = true
        while more do {
          val l = c.readLine(4096).getOrElse(throw new java.io.EOFException(s"$host が接続を閉じました"))
          lines += l
          more = l.length > 3 && l(3) == '-'
        }
        (lines.last.take(3).toIntOption.getOrElse(0), lines.mkString(" / "))
      }
      def cmd(s: String): (Int, String) = { c.write(s + "\r\n"); read() }
      def expect(r: (Int, String), ok: Int => Boolean): Unit =
        if !ok(r._1) then throw SmtpReplyError(r._1, r._2)
      try {
        expect(read(), _ == 220)
        var ehlo = cmd(s"EHLO ${cfg.hostname}")
        expect(ehlo, _ == 250)
        val canTls = ehlo._2.toUpperCase.contains("STARTTLS")
        if !c.isTls && canTls && tryTls then {
          expect(cmd("STARTTLS"), _ == 220)
          c.startClientTls(host, verify = cfg.outboundVerifyTls || relay.isDefined)
          ehlo = cmd(s"EHLO ${cfg.hostname}")
          expect(ehlo, _ == 250)
        } else if !c.isTls && (cfg.outboundRequireTls || relay.exists(_.tls == "starttls")) then
          throw SmtpReplyError(421, s"$host は STARTTLS に対応していません")
        relay.flatMap(r => r.user.map(_ -> r.password.getOrElse(""))).foreach {
          (u, p) =>
            {
              val token = Base64.getEncoder.encodeToString(s"\u0000$u\u0000$p".getBytes(StandardCharsets.UTF_8))
              expect(cmd(s"AUTH PLAIN $token"), _ == 235)
            }
        }
        val size = if ehlo._2.toUpperCase.contains("SIZE") then s" SIZE=${raw.length}" else ""
        val body8 = if ehlo._2.toUpperCase.contains("8BITMIME") && raw.exists(_ < 0) then " BODY=8BITMIME" else ""
        expect(cmd(s"MAIL FROM:<$sender>$size$body8"), _ == 250)
        val results = rcpts.map { r =>
          val (code, text) = cmd(s"RCPT TO:<$r>")
          r ->
            (if code / 100 == 2 then RcptResult.Ok
             else if code / 100 == 4 then RcptResult.Temp(text) else RcptResult.Perm(text))
        }.toMap
        if !results.values.exists(_ == RcptResult.Ok) then { cmd("QUIT"); results }
        else {
          expect(cmd("DATA"), _ == 354)
          c.writeBytes(dotStuff(raw))
          c.writeRaw(".\r\n")
          c.flush()
          val (code, text) = read()
          cmd("QUIT")
          results.map {
            (r, v) =>
              r ->
                (if v != RcptResult.Ok then v
                 else if code / 100 == 2 then RcptResult.Ok
                 else if code / 100 == 4 then RcptResult.Temp(text) else RcptResult.Perm(text))
          }
        }
      } catch {
        case SmtpReplyError(code, text) =>
          val res = if code / 100 == 5 then RcptResult.Perm(text) else RcptResult.Temp(text)
          rcpts.map(_ -> res).toMap
      }
    } finally c.close()
  }

  private def dotStuff(raw: Array[Byte]): Array[Byte] = {
    val s = new String(Compose.crlf(raw), StandardCharsets.ISO_8859_1)
    val stuffed = s.split("\r\n", -1).map(l => if l.startsWith(".") then "." + l else l).mkString("\r\n")
    (if stuffed.endsWith("\r\n") then stuffed else stuffed + "\r\n").getBytes(StandardCharsets.ISO_8859_1)
  }
}
