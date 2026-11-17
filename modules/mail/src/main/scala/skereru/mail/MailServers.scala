package skereru.mail

import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import skereru.db.*

/** SMTP・IMAP・POP3 の待ち受けをまとめて起動する。 */
object MailServers:

  final case class Deps(cfg: MailConfig, tls: Option[TlsHolder], delivery: Delivery, auth: MailAuth, mail: MailStore, blobs: skereru.storage.Blobs,
      queue: QueueStore, dns: JndiDns, log: String => Unit)

  def start(d: Deps): Resource[IO, ConnLimits] =
    import d.*
    val limits = new ConnLimits(cfg.maxConnections, cfg.maxConnectionsPerIp)
    for
      disp <- Dispatcher.parallel[IO](await = false)
      b = new Bridge(disp)
      smtp = (mode: SmtpMode) => (c: Conn) => new SmtpSession(c, mode, cfg, tls, b, delivery, auth, queue, log).run()
      imap = (c: Conn) => new ImapSession(c, cfg, tls, b, mail, blobs, queue, auth, log).run()
      pop3 = (c: Conn) => new Pop3Session(c, cfg, tls, b, mail, blobs, auth).run()
      listeners = List(
        cfg.smtpPort.map(p => (Listener("smtp", p, false), smtp(SmtpMode.Mx))),
        cfg.submissionPort.map(p => (Listener("submission", p, false), smtp(SmtpMode.Submission))),
        cfg.smtpsPort.map(p => (Listener("smtps", p, true), smtp(SmtpMode.Submission))),
        cfg.imapPort.map(p => (Listener("imap", p, false), imap)),
        cfg.imapsPort.map(p => (Listener("imaps", p, true), imap)),
        cfg.pop3Port.map(p => (Listener("pop3", p, false), pop3)),
        cfg.pop3sPort.map(p => (Listener("pop3s", p, true), pop3))
      ).flatten.filter((l, _) => !l.implicitTls || tls.isDefined)
      _ <- listeners.traverse_((l, h) => TcpServer.resource(l, cfg.bind, tls, limits, cfg.proxyFrom, log)(h))
      _ <- Resource.eval(IO(log(s"[mail] listening: ${listeners.map((l, _) => s"${l.name}:${l.port}").mkString(" ")}")))
    yield limits
