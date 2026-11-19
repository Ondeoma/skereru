package skereru.mail

import cats.effect.*
import jakarta.mail.*
import jakarta.mail.internet.*
import munit.CatsEffectSuite
import skereru.auth.{AppPasswords, SecretBox}
import skereru.core.*
import skereru.db.*
import skereru.shared.*
import skereru.storage.*

import java.net.{ServerSocket, Socket}
import java.nio.charset.StandardCharsets
import java.util.{Properties, UUID}

/** SMTP・IMAP・POP3 の結合テスト。TEST_DATABASE_URL がなければ飛ばす（例: jdbc:postgresql://localhost:54370/skereru_test）。 */
class MailSpec extends CatsEffectSuite {

  private val dbCfg: Option[DbConfig] = sys.env.get("TEST_DATABASE_URL").map(url =>
    DbConfig(
      url,
      sys.env.getOrElse("TEST_DATABASE_USER", "skereru"),
      sys.env.getOrElse("TEST_DATABASE_PASSWORD", "skereru")
    )
  )

  private def freePort(): Int = {
    val s = new ServerSocket(0);
    try s.getLocalPort
    finally s.close()
  }

  final case class Env(
    cfg: MailConfig,
    users: UserStore,
    domains: DomainStore,
    mail: MailStore,
    queue: QueueStore,
    domain: String,
    alice: (UUID, String, String),
    bob: (UUID, String, String),
    carol: (UUID, String, String),
    imports: ImportStore,
    importer: Importer,
    dmarc: DmarcStore)

  private val secret = "test-app-secret-0123456789"

  private val envFixture = ResourceSuiteLocalFixture(
    "env",
    dbCfg.fold(Resource.eval(IO.raiseError[Env](new RuntimeException("no db")))) { c =>
      for {
        _ <- Resource.eval(Database.migrate(c))
        xa <- Database.transactor(c)
        users = new UserStore(xa); domains = new DomainStore(xa); mail = new MailStore(xa); queue = new QueueStore(xa)
        cfg = MailConfig(
          hostname = "mx.test.local",
          bind = "127.0.0.1",
          smtpPort = Some(freePort()),
          submissionPort = Some(freePort()),
          smtpsPort = Some(freePort()),
          imapPort = Some(freePort()),
          imapsPort = Some(freePort()),
          pop3Port = Some(freePort()),
          pop3sPort = Some(freePort()),
          dnsServers = Nil,
          maxMessageBytes = 5 * 1024 * 1024
        )
        log = (s: String) => println(s)
        tls = Some(new TlsHolder(None, None, "mx.test.local", log))
        dns = new JndiDns(Nil, 1000)
        box = SecretBox(secret)
        blobs = new PgBlobs(mail, new BlobCrypto(secret, true), new BlobCache(10 * 1024 * 1024))
        spam = new SpamFilter(dns, queue, cfg, log)
        delivery =
          new Delivery(mail, domains, users, queue, blobs, spam, new Signer(domains, box), cfg, log, _ => IO.unit)
        // テスト用のドメインとユーザー
        dname = s"t${UUID.randomUUID().toString.take(8)}.example"
        (priv, pub) = Dkim.generate()
        did <- Resource.eval(domains.createDomain(
          dname,
          false,
          "s1",
          box.encrypt(java.util.Base64.getEncoder.encodeToString(priv)),
          pub
        ).map(_.toOption.get))
        mk = (name: String) =>
          for {
            uid <- users.devUser(s"$name-$dname", None, false).map(_.id)
            _ <- domains.createAddressIO(s"$name@$dname", did, Some(uid), Nil, true, true)
            _ <- mail.ensureDefaults(uid)
            (shown, prefix) = AppPasswords.generate()
            _ <- users.createAppPassword(uid, "test", AppPasswords.hash(shown), prefix)
          } yield (uid, s"$name@$dname", shown)
        alice <- Resource.eval(mk("alice"))
        bob <- Resource.eval(mk("bob"))
        carol <- Resource.eval(mk("carol"))
        imports = new ImportStore(xa)
        // 取り込みの試験は同じ機械の IMAP サーバーを相手にするので、内部のアドレス・自己署名の証明書を許す
        importer =
          new Importer(mail, imports, blobs, box, dns, cfg.copy(outboundAllowPrivate = true), log, _ => IO.unit)
        _ <- Resource.eval(domains.createAddressIO(
          s"team@$dname",
          did,
          None,
          List(s"alice@$dname", s"bob@$dname"),
          false,
          false
        ))
        _ <- Resource.eval(delivery.refreshDomains)
        dmarc = new DmarcStore(xa)
        _ = delivery.onDmarcReport = (
          raw,
          authed
        ) =>
          DmarcReport.fromMessage(raw).fold(e => IO(log(s"dmarc: $e")), a => dmarc.save(a, authed))
        _ <- MailServers.start(MailServers.Deps(cfg, tls, delivery, new MailAuth(users), mail, blobs, queue, dns, log))
      } yield Env(cfg, users, domains, mail, queue, dname, alice, bob, carol, imports, importer, dmarc)
    }
  )

  override def munitFixtures = if dbCfg.isDefined then List(envFixture) else Nil

  private def withEnv(
    name: String
  )(
    body: Env => IO[Unit]
  ): Unit =
    test(name)(if dbCfg.isEmpty then IO(assume(false, "TEST_DATABASE_URL が未設定")) else body(envFixture()))

  private def props(
    e: Env
  ): Properties = {
    val p = new Properties()
    p.put("mail.smtps.host", "127.0.0.1"); p.put("mail.smtps.port", e.cfg.smtpsPort.get.toString);
    p.put("mail.smtps.auth", "true")
    p.put("mail.smtps.ssl.trust", "*"); p.put("mail.smtps.ssl.checkserveridentity", "false")
    p.put("mail.smtp.host", "127.0.0.1"); p.put("mail.smtp.port", e.cfg.submissionPort.get.toString);
    p.put("mail.smtp.auth", "true")
    p.put("mail.smtp.starttls.enable", "true"); p.put("mail.smtp.starttls.required", "true");
    p.put("mail.smtp.ssl.trust", "*")
    p.put("mail.imaps.ssl.trust", "*"); p.put("mail.imaps.ssl.checkserveridentity", "false");
    p.put("mail.imaps.port", e.cfg.imapsPort.get.toString)
    p.put("mail.pop3s.ssl.trust", "*"); p.put("mail.pop3s.port", e.cfg.pop3sPort.get.toString)
    p
  }

  private def send(
    e: Env,
    protocol: String,
    from: (UUID, String, String),
    to: List[String],
    subject: String,
    body: String,
    fromHeader: Option[String] = None
  ): IO[Unit] =
    IO.blocking {
      val s = Session.getInstance(props(e))
      val m = new MimeMessage(s)
      m.setFrom(new InternetAddress(fromHeader.getOrElse(from._2)))
      m.setRecipients(Message.RecipientType.TO, to.mkString(","))
      m.setSubject(subject, "UTF-8")
      m.setText(body, "UTF-8")
      val t = s.getTransport(protocol)
      t.connect("127.0.0.1", from._2, from._3)
      try t.sendMessage(m, m.getAllRecipients)
      finally t.close()
    }

  private def imap[A](
    e: Env,
    who: (UUID, String, String)
  )(
    f: Store => A
  ): IO[A] = IO.blocking {
    val st = Session.getInstance(props(e)).getStore("imaps")
    st.connect("127.0.0.1", who._2, who._3)
    try f(st)
    finally st.close()
  }

  withEnv("submission で送ったメールを IMAP で読み、フラグ・移動・検索ができる") { e =>
    for {
      _ <- send(e, "smtps", e.alice, List(e.bob._2), "日本語の件名テスト", "本文です。\n.先頭がドットの行\n")
      r <- imap(e, e.bob) { st =>
        val inbox = st.getFolder("INBOX")
        inbox.open(Folder.READ_WRITE)
        val msgs = inbox.getMessages
        assertEquals(msgs.length, 1)
        val m = msgs.head.asInstanceOf[MimeMessage]
        assertEquals(m.getSubject, "日本語の件名テスト")
        assert(m.getHeader("DKIM-Signature") != null, "DKIM で署名されている")
        assertEquals(m.getContent.toString.replace("\r\n", "\n"), "本文です。\n.先頭がドットの行\n")
        assert(m.isSet(Flags.Flag.SEEN), "本文を読んだら既読")
        m.setFlag(Flags.Flag.FLAGGED, true)
        // フォルダーを作って移動（日本語の名前）
        val work = st.getFolder("仕事/請求")
        assert(work.create(Folder.HOLDS_MESSAGES))
        inbox.copyMessages(Array(m), work)
        m.setFlag(Flags.Flag.DELETED, true)
        inbox.expunge()
        assertEquals(inbox.getMessageCount, 0)
        inbox.close(false)
        work.open(Folder.READ_ONLY)
        val found = work.search(new jakarta.mail.search.SubjectTerm("件名"))
        val flagged = work.getMessages.head.isSet(Flags.Flag.FLAGGED)
        val names = st.getDefaultFolder.list("*").map(_.getFullName).toSet
        work.close(false)
        (found.length, flagged, names)
      }
      _ = assertEquals(r._1, 1)
      _ = assert(r._2, "フラグはコピー先にも残る")
      _ = assert(Set("INBOX", "Sent", "Junk", "Trash", "Drafts", "仕事", "仕事/請求").subsetOf(r._3), r._3)
    } yield ()
  }

  withEnv("ほかの人のアドレスを From にして送ることはできない") { e =>
    send(e, "smtp", e.alice, List(e.bob._2), "なりすまし", "x", fromHeader = Some(e.bob._2)).attempt.map { r =>
      assert(r.isLeft, "From が自分のアドレスでなければ拒否")
    }
  }

  withEnv("間違ったパスワードでは IMAP にログインできない") { e =>
    imap(e, e.bob.copy(_3 = "wrong-password"))(_ => ()).attempt.map(r =>
      assert(r.left.exists(_.isInstanceOf[AuthenticationFailedException]), r)
    )
  }

  /** 生の SMTP の会話。 */
  private def smtpTalk(
    e: Env,
    lines: List[String]
  ): IO[List[String]] = IO.blocking {
    val s = new Socket("127.0.0.1", e.cfg.smtpPort.get)
    s.setSoTimeout(30_000)
    val in = new java.io.BufferedReader(new java.io.InputStreamReader(s.getInputStream, StandardCharsets.UTF_8))
    val out = s.getOutputStream
    def read(): String = {
      var l = in.readLine()
      val all = new StringBuilder(l)
      while l != null && l.length > 3 && l(3) == '-' do { l = in.readLine(); all.append(" | ").append(l) }
      all.toString
    }
    try {
      val replies = scala.collection.mutable.ListBuffer(read())
      lines.foreach { l =>
        out.write(l.getBytes(StandardCharsets.UTF_8)); out.flush()
        if !l.startsWith("BODY:") then replies += read()
      }
      replies.toList
    } finally s.close()
  }

  withEnv("MX: 中継の拒否・存在しない宛先・単独の LF の拒否・別名への配送") { e =>
    for {
      r <- smtpTalk(
        e,
        List(
          "EHLO client.example.org\r\n",
          "MAIL FROM:<sender@sender.invalid>\r\n",
          "RCPT TO:<someone@other.example.com>\r\n",
          s"RCPT TO:<nobody@${e.domain}>\r\n",
          s"RCPT TO:<team@${e.domain}>\r\n",
          "DATA\r\n",
          "Subject: smuggle\r\n\r\nline\n.\r\nMAIL FROM:<x@evil.example>\r\n.\r\n" // "\n.\r\n" で別のメッセージを紛れ込ませる
        )
      )
      _ = assert(
        r(1).startsWith("250") && r(1).contains("PIPELINING") && r(1).contains("STARTTLS") && !r(1).contains("AUTH"),
        r(1)
      )
      _ = assert(r(3).startsWith("554"), s"中継は拒否: ${r(3)}")
      _ = assert(r(4).startsWith("550"), s"存在しない宛先: ${r(4)}")
      _ = assert(r(5).startsWith("250"), r(5))
      _ = assert(r(7).startsWith("550 5.6.11"), s"単独の LF: ${r(7)}")
      // 単独の LF の行は（"." と隣り合わなければ）CRLF に直して受け付ける
      lf <- smtpTalk(
        e,
        List(
          "EHLO client.example.org\r\n",
          "MAIL FROM:<sender@sender.invalid>\r\n",
          s"RCPT TO:<bob@${e.domain}>\r\n",
          "DATA\r\n",
          "Subject: lf\n\nhello\nworld\r\n.\r\n",
          "QUIT\r\n"
        )
      )
      _ = assert(lf(5).startsWith("250"), lf.mkString("\n"))
      ok <- smtpTalk(
        e,
        List(
          "EHLO client.example.org\r\n",
          "MAIL FROM:<sender@sender.invalid>\r\n",
          s"RCPT TO:<team@${e.domain}>\r\n",
          "DATA\r\n",
          s"From: Sender <sender@sender.invalid>\r\nTo: team@${e.domain}\r\nSubject: =?UTF-8?B?44OB44O844Og5a6b?=\r\nMessage-ID: <x1@sender.invalid>\r\nDate: Mon, 16 Nov 2026 10:00:00 +0900\r\n\r\nhello team\r\n.\r\n",
          "QUIT\r\n"
        )
      )
      _ = assert(ok(5).startsWith("250"), ok.mkString("\n"))
      a <- e.mail.special(e.alice._1, SpecialUse.Inbox).flatMap(b =>
        e.mail.list(e.alice._1, b.map(_.id), MailQuery.parse(""), false, 0, 10, e.cfg.zone)
      )
      bb <- e.mail.special(e.bob._1, SpecialUse.Inbox).flatMap(b =>
        e.mail.list(e.bob._1, b.map(_.id), MailQuery.parse("チーム"), false, 0, 10, e.cfg.zone)
      )
      _ = assert(a._1.exists(_._1.subject == "チーム宛"), a)
      _ = assertEquals(bb._1.map(_._1.subject), List("チーム宛"))
      _ = assert(a._1.head._1.authResults.isDefined)
    } yield ()
  }

  withEnv("POP3 で読み、削除できる") { e =>
    for {
      _ <- send(e, "smtps", e.bob, List(e.alice._2), "POP3 テスト", "pop body")
      n <- IO.blocking {
        val st = Session.getInstance(props(e)).getStore("pop3s")
        st.connect("127.0.0.1", e.alice._2, e.alice._3)
        try {
          val f = st.getFolder("INBOX")
          f.open(Folder.READ_WRITE)
          val ms = f.getMessages
          val target = ms.find(_.getSubject == "POP3 テスト").get
          assertEquals(target.getContent.toString.trim, "pop body")
          target.setFlag(Flags.Flag.DELETED, true)
          val before = ms.length
          f.close(true)
          before
        } finally st.close()
      }
      left <- e.mail.special(e.alice._1, SpecialUse.Inbox).flatMap(b =>
        e.mail.list(e.alice._1, b.map(_.id), MailQuery.parse(""), false, 0, 50, e.cfg.zone)
      )
      _ = assertEquals(left._2, n - 1)
    } yield ()
  }

  withEnv("外への宛先は送信待ちに入る") { e =>
    for {
      _ <- send(e, "smtps", e.alice, List("someone@example.invalid"), "外へ", "out")
      q <- e.queue.list(Some("queued"), 50)
      _ = assert(q.exists(x => x.recipient == "someone@example.invalid" && x.sender == e.alice._2), q)
    } yield ()
  }

  /** IMAPS に生のコマンドを送る（証明書は確かめない）。send は1行を送り、tag で始まる応答の行を返す。 */
  private def rawImaps[A](
    e: Env
  )(
    f: (String => Unit, String => String) => A
  ): A = {
    val ctx = javax.net.ssl.SSLContext.getInstance("TLS")
    ctx.init(
      null,
      Array[javax.net.ssl.TrustManager](new javax.net.ssl.X509TrustManager {
        def checkClientTrusted(
          c: Array[java.security.cert.X509Certificate],
          a: String
        ) = ()
        def checkServerTrusted(
          c: Array[java.security.cert.X509Certificate],
          a: String
        ) = ()
        def getAcceptedIssuers = Array.empty[java.security.cert.X509Certificate]
      }),
      null
    )
    val s = ctx.getSocketFactory.createSocket("127.0.0.1", e.cfg.imapsPort.get)
    s.setSoTimeout(10_000)
    val in = new java.io.BufferedReader(new java.io.InputStreamReader(s.getInputStream, StandardCharsets.UTF_8))
    def send(
      l: String
    ): Unit = { s.getOutputStream.write((l + "\r\n").getBytes(StandardCharsets.UTF_8)); s.getOutputStream.flush() }
    def until(
      tag: String
    ): String =
      Iterator.continually(in.readLine()).find(l => l == null || l.startsWith(tag) || l.startsWith("* BYE")).orNull
    try { in.readLine(); f(send, until) }
    finally s.close()
  }

  withEnv("セキュリティ: ログインの前に大きなリテラルを宣言しても受け付けない") { e =>
    IO.blocking(rawImaps(e) {
      (
        send,
        until
      ) =>
        {
          send("a1 LOGIN {52428800+}")
          val r = until("a1")
          assert(r != null && (r.contains("BAD") || r.contains("BYE")), r)
        }
    })
  }

  withEnv("セキュリティ: 桁あふれするリテラルの長さ・* を並べた LIST で止まらない") { e =>
    IO.blocking(rawImaps(e) {
      (
        send,
        until
      ) =>
        {
          send("a0 LOGIN {9223372036854775807+}")
          val r0 = until("a0")
          assert(r0 != null && (r0.contains("BAD") || r0.contains("BYE")), r0)
        }
    }) *> IO.blocking(rawImaps(e) {
      (
        send,
        until
      ) =>
        {
          send(s"""a1 LOGIN "${e.alice._2}" "${e.alice._3}"""")
          val r1 = until("a1"); assert(r1 != null && r1.startsWith("a1 OK"), r1)
          val t = System.nanoTime()
          send("a2 LIST \"\" \"" + "*" * 40 + "z\"")
          assert(until("a2").startsWith("a2 OK"))
          assert((System.nanoTime() - t) / 1_000_000 < 2000)
        }
    })
  }

  /** user のメールボックス name（特別なものは special_use）の件数。 */
  private def count(
    e: Env,
    user: UUID,
    box: IO[Option[Mailbox]]
  ): IO[Int] =
    box.flatMap(b => e.mail.list(user, b.map(_.id), MailQuery.parse(""), false, 0, 500, e.cfg.zone)).map(_._2)

  withEnv("取り込み: IMAP で、ほかのサーバーのメールを取り込む（2回目は重複として飛ばす）") { e =>
    for {
      _ <- send(e, "smtps", e.alice, List(e.bob._2), "取り込み 1", "one")
      _ <- send(e, "smtps", e.alice, List(e.bob._2), "取り込み 2", "two")
      src = ImapSource("127.0.0.1", e.cfg.imapsPort.get, e.bob._2, e.bob._3)
      bad <- e.importer.startImap(e.carol._1, src.copy(password = "wrong"))
      _ = assert(bad.left.exists(_.contains("ログインできません")), bad)
      job <- e.importer.startImap(e.carol._1, src).map(_.fold(err => fail(err), identity))
      _ <- e.importer.runNext("test")
      done <- e.imports.find(e.carol._1, job.id)
      bobInbox <- count(e, e.bob._1, e.mail.special(e.bob._1, SpecialUse.Inbox))
      carolInbox <- count(e, e.carol._1, e.mail.special(e.carol._1, SpecialUse.Inbox))
      _ = assertEquals(done.map(_.status), Some("done"), done)
      _ = assertEquals(carolInbox, bobInbox)
      _ = assert(done.exists(_.imported >= bobInbox), done)
      // やり直しても、同じメールは取り込まない
      job2 <- e.importer.startImap(e.carol._1, src).map(_.fold(err => fail(err), identity))
      _ <- e.importer.runNext("test")
      again <- e.imports.find(e.carol._1, job2.id)
      carolInbox2 <- count(e, e.carol._1, e.mail.special(e.carol._1, SpecialUse.Inbox))
      _ = assertEquals(carolInbox2, carolInbox)
      _ = assertEquals(again.map(_.imported), Some(0), again)
    } yield ()
  }

  withEnv("取り込み: Google Takeout の mbox はラベルのフォルダ・スター・未読を引き継ぐ") { e =>
    val msg =
      "X-Gmail-Labels: Inbox,Starred,Unread,\"仕事/案件\"\nFrom: x@example.org\nTo: carol@example.org\nSubject: takeout\nMessage-ID: <takeout-1@example.org>\nDate: Mon, 16 Nov 2026 10:00:00 +0900\n\nhello\n"
        .getBytes(StandardCharsets.UTF_8)
    val framed = java.nio.ByteBuffer.allocate(4 + msg.length).putInt(msg.length).put(msg).array()
    for {
      job <- e.imports.create(e.carol._1, "file", "takeout.mbox", None, "running").map(_.toOption.get)
      r <- e.importer.importBatch(e.carol._1, job.id, framed, "取り込み/takeout")
      _ = assertEquals(r.map(_.imported), Right(1), r)
      _ <- e.imports.finish(job.id, "done", None)
      work <- e.mail.mailboxByName(e.carol._1, "仕事/案件")
      _ = assert(work.isDefined, "ラベルのフォルダができていない")
      list <- e.mail.list(e.carol._1, work.map(_.id), MailQuery.parse(""), false, 0, 10, e.cfg.zone)
      m = list._1.head._1
      _ = assert(m.flags.contains(Flag.Flagged) && !m.flags.contains(Flag.Seen), m.flags)
      // ほかのファイルの取り込みは、1人1つまで
      j1 <- e.imports.create(e.carol._1, "file", "a.mbox", None, "running")
      j2 <- e.imports.create(e.carol._1, "file", "b.mbox", None, "running")
      _ = assert(j1.isRight && j2.isLeft)
      _ <- e.imports.cancel(e.carol._1, j1.toOption.get.id)
    } yield ()
  }

  test("取り込み: 送られたデータの分け方（形が正しくなければ受け付けない）") {
    val a = "abc".getBytes; val b = "hello".getBytes
    val framed =
      java.nio.ByteBuffer.allocate(8 + a.length + b.length).putInt(a.length).put(a).putInt(b.length).put(b).array()
    assertEquals(Importer.unframe(framed).map(_.map(new String(_))), Some(List("abc", "hello")))
    assertEquals(Importer.unframe(framed.dropRight(1)), None)
    assertEquals(Importer.unframe(Array[Byte](0x7f, 0, 0, 0)), None)
  }

  withEnv("DMARC: dmarc-reports@ に届いた集計レポートを保存する（受信トレイには入れない）") { e =>
    val id = s"rep-${UUID.randomUUID().toString.take(8)}"
    val xml =
      s"""<?xml version="1.0"?><feedback><report_metadata><org_name>example.net</org_name><email>noreply@example.net</email>
         |<report_id>$id</report_id><date_range><begin>1763251200</begin><end>1763337599</end></date_range></report_metadata>
         |<policy_published><domain>${e.domain}</domain><p>quarantine</p></policy_published>
         |<record><row><source_ip>192.0.2.1</source_ip><count>5</count><policy_evaluated><disposition>none</disposition><dkim>pass</dkim><spf>pass</spf></policy_evaluated></row>
         |<identifiers><header_from>${e.domain}</header_from></identifiers></record>
         |<record><row><source_ip>198.51.100.7</source_ip><count>2</count><policy_evaluated><disposition>quarantine</disposition><dkim>fail</dkim><spf>fail</spf></policy_evaluated></row>
         |<identifiers><header_from>${e.domain}</header_from></identifiers></record></feedback>""".stripMargin
    val raw =
      s"From: noreply@example.net\r\nTo: dmarc-reports@${e.domain}\r\nSubject: Report Domain: ${e.domain}\r\nMessage-ID: <$id@example.net>\r\n" +
        s"Date: Mon, 16 Nov 2026 10:00:00 +0000\r\nContent-Type: text/xml\r\n\r\n" + xml.replace("\n", "\r\n") +
        "\r\n"
    for {
      _ <- IO.blocking {
        // 外からの受信（25番）として送る
        val s = new Socket("127.0.0.1", e.cfg.smtpPort.get)
        s.setSoTimeout(10_000)
        val in = new java.io.BufferedReader(new java.io.InputStreamReader(s.getInputStream, StandardCharsets.UTF_8))
        val out = s.getOutputStream
        def cmd(
          l: String
        ): String = {
          out.write((l + "\r\n").getBytes(StandardCharsets.UTF_8)); out.flush();
          Iterator.continually(in.readLine()).find(x => x == null || x.length < 4 || x(3) == ' ').orNull
        }
        try {
          in.readLine()
          cmd("EHLO mail.example.net"); cmd("MAIL FROM:<noreply@example.net>")
          val r = cmd(s"RCPT TO:<dmarc-reports@${e.domain}>")
          assert(r.startsWith("250"), r)
          cmd("DATA")
          val d = cmd(raw + ".")
          assert(d.startsWith("250"), d)
          cmd("QUIT")
        } finally s.close()
      }
      list <- e.dmarc.list(Some(e.domain))
      rep = list.find(_.reportId == id)
      _ = assert(rep.isDefined, list)
      _ = assertEquals(rep.map(r => (r.total, r.passing, r.policy)), Some((7L, 5L, "quarantine")))
      rows <- e.dmarc.rows(rep.get.id)
      _ = assertEquals(rows.map(_.map(_.sourceIp).sorted), Some(List("192.0.2.1", "198.51.100.7")))
    } yield ()
  }
}
