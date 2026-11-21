package skereru.mail

import sbirds.observe.Log

import skereru.core.ErrorCode

import cats.effect.*
import cats.syntax.all.*
import skereru.core.*
import skereru.db.*
import sbirds.observe.{Metrics, Telemetry}
import skereru.shared.*

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.UUID

/** 配送（受け取ったメッセージを宛先のメールボックスに入れる・外へ送る）。 */
final class Delivery(mail: MailStore,
                     domains: DomainStore,
                     users: UserStore,
                     queue: QueueStore,
                     blobs: skereru.storage.Blobs,
                     spam: SpamFilter,
                     signer: Signer,
                     cfg: MailConfig,
                     log: String => Unit,
                     notify: UUID => IO[Unit]) {

  /** DMARC の集計レポートを受け取ったとき（起動時に保存先をつなぐ）。 */
  @volatile var onDmarcReport: (Array[Byte], Boolean) => IO[Unit] = (_, _) => IO.unit

  /** 受信トレイに新しいメールが届いたとき（ユーザー・メッセージ・解析した本文. 起動時にプッシュ通知をつなぐ）。待たずに裏で動かす。 */
  @volatile var onInboxMail: (UUID, UUID, MimePart) => IO[Unit] = (_, _, _) => IO.unit

  /**
   * メールを保存したとき（迷惑メール・ゴミ箱へ入れたものを除く. 起動時に stui・Webhook への通知と skawau への保存をつなぐ）。待たずに裏で動かす。
   * 引数: ユーザー・メッセージ・解析した本文・入れたメールボックスの名前・受信トレイの新着か・振り分けの規則の結果。
   */
  @volatile var onMailStored: (UUID, UUID, MimePart, String, Boolean, Rules.Outcome) => IO[Unit] =
    (_, _, _, _, _, _) => IO.unit

  /** アドレスが配送先として存在するか（+tag・キャッチオールも見る）。転送先は再帰的にたどる。 */
  def resolve(rcpt: String, depth: Int = 0): IO[Option[Target]] = {
    val a = rcpt.toLowerCase
    domains.address(a).flatMap {
      case Some(r) => Some(r).pure[IO]
      case None => if Address.withoutTag(a) != a then domains.address(Address.withoutTag(a)) else IO.pure(None)
    }.flatMap {
      case Some(r) =>
        // 転送先のうち、このサーバーのアドレスはたどって配送先のユーザーにする
        r.forwardTo.traverse { f =>
          if depth >= 5 then IO.pure(Target(Nil, Nil))
          else
            domains.isLocalDomain(Address.domainOf(f)).flatMap {
              case true => resolve(f, depth + 1).map(_.getOrElse(Target(Nil, Nil)))
              case false => IO.pure(Target(Nil, List(f)))
            }
        }.map { ts => Some(Target((r.userId.toList ++ ts.flatMap(_.users)).distinct, ts.flatMap(_.forwards).distinct)) }
      case None =>
        // DMARC の集計レポートの受け口（アドレスを作っていなくても受け取る）
        if Address.localOf(a) == Delivery.DmarcReportsLocal && isLocal(Address.domainOf(a)) then
          IO.pure(Some(Target(Nil, Nil, dmarcReports = true)))
        else domains.domainByName(Address.domainOf(a)).map(_.flatMap(_.catchAll).map(u => Target(List(u), Nil)))
    }
  }

  def isLocalDomain(d: String): IO[Boolean] = domains.isLocalDomain(d)

  /** 受信の前に足すヘッダー。 */
  private def received(env: Envelope, rdns: Option[String], proto: String, forRcpt: Option[String]): String = {
    val from =
      s"${env.helo.take(100).replaceAll("[^A-Za-z0-9.\\-\\[\\]:]", "")} (${rdns.getOrElse("unknown")} [${env.ip.getHostAddress}])"
    s"Received: from $from\r\n\tby ${cfg.hostname} (skereru) with $proto\r\n\t${forRcpt.fold("")(r => s"for <$r>")}; ${Dates.rfc5322(Instant.now(), cfg.zone)}\r\n"
  }

  /** 外から受け取ったメッセージ（ポート25）。迷惑メールの判定をして、宛先のユーザーに配る。 */
  def inbound(env: Envelope, raw0: Array[Byte]): IO[InboundResult] =
    Telemetry.span("mail.inbound", "mail.rcpts" -> env.rcpts.size.toString)(inboundImpl(env, raw0)).flatTap {
      case InboundResult.Accepted(_) => IO.unit // 配った結果（delivered・junk）は inboundImpl で数える
      case InboundResult.Rejected(_, _) => Metrics.count("skereru.mail.received", "result" -> "rejected")
      case InboundResult.TempFail(_, _) => Metrics.count("skereru.mail.received", "result" -> "tempfail")
    }

  private def inboundImpl(env: Envelope, raw0: Array[Byte]): IO[InboundResult] = {
    val root0 = Mime.parse(raw0)
    // 転送が回り続けているメール（Received が多すぎる）は受け取らない
    if root0.headerAll("Received").size > Delivery.MaxHops then
      return Metrics.count(
        "skereru.mail.rejected",
        "reason" -> "loop"
      ).as(InboundResult.Rejected("554 5.4.6", "Too many hops (mail loop detected)"))
    for {
      targets <- env.rcpts.traverse(r => resolve(r).map(r -> _))
      userIds = targets.flatMap(_._2.toList.flatMap(_.users)).distinct
      res <- spam.check(raw0, root0, env.ip, env.helo, env.mailFrom, userIds.headOption, d => isLocal(d))
      // 送信元の認証（SPF・DKIM・DMARC）の結果の内訳
      _ <- Metrics.count(
        "skereru.mail.auth",
        "spf" -> Delivery.word(res.auth.spf),
        "dkim" -> Delivery.word(res.auth.dkim),
        "dmarc" -> Delivery.word(res.auth.dmarc)
      )
      out <- res.verdict.action match {
        case SpamAction.Reject =>
          Metrics.count("skereru.mail.rejected", "reason" -> Delivery.rejectReason(res.verdict)) *> queue.log(
            "in",
            "rejected",
            Some(env.ip.getHostAddress),
            env.mailFrom,
            env.rcpts,
            raw0.length,
            Some(res.verdict.score),
            res.verdict.summary
          )
            .as(InboundResult.Rejected("550 5.7.1", s"Message rejected as spam (score ${res.verdict.score})"))
        case action =>
          val proto = if env.tls then "ESMTPS" else "ESMTP"
          val spamHeaders =
            s"X-Spam-Score: ${res.verdict.score}\r\nX-Spam-Status: ${
                if action == SpamAction.Junk then "Yes" else "No"
              }, score=${res.verdict.score} tests=${res.verdict.hits.map(_.rule).mkString(",")}\r\n"
          val head = s"Return-Path: <${env.mailFrom}>\r\n" +
            received(env, None, proto, env.rcpts.headOption.filter(_ => env.rcpts.size == 1)) + res.header + spamHeaders
          val raw = head.getBytes(StandardCharsets.ISO_8859_1) ++ stripSpoofable(raw0)
          for {
            id <- blobs.put(raw)
            results <- userIds.traverse(u =>
              deliverToUsers(List(u), raw, id, Some(res), env.mailFrom, junk = action == SpamAction.Junk)
            ).map(_.flatten)
            fwds = targets.flatMap(_._2.toList.flatMap(_.forwards)).distinct
            // DMARC の集計レポートは、解析して保存する（受信トレイには入れない）。認証されたかどうかも残す
            _ <- IO.whenA(targets.exists(_._2.exists(_.dmarcReports)))(onDmarcReport(
              raw0,
              res.auth.spf.startsWith("pass") || res.auth.dkim.startsWith("pass")
            )
              .handleErrorWith(e => Log.report(ErrorCode.DmarcReportFailed, e.toString, e)))
            _ <- IO.whenA(fwds.nonEmpty && action != SpamAction.Junk)(forward(
              env.rcpts.headOption.getOrElse(""),
              env.mailFrom,
              fwds,
              raw,
              Some(res.header)
            ))
            // 誰も受け取れなかった（容量の上限など）なら、黙って捨てずに一時エラーにして、相手に再送・通知してもらう
            allFailed = results.nonEmpty && results.forall(_._2.isLeft) && fwds.isEmpty
            _ <- IO.whenA(!allFailed)(Metrics.count(
              "skereru.mail.received",
              "result" -> (if action == SpamAction.Junk then "junk" else "delivered")
            ))
            _ <- queue.log(
              "in",
              if allFailed then "tempfail" else if action == SpamAction.Junk then "junk" else "delivered",
              Some(env.ip.getHostAddress),
              env.mailFrom,
              env.rcpts,
              raw.length,
              Some(res.verdict.score),
              if allFailed then results.flatMap(_._2.left.toOption).mkString(" | ") else res.verdict.summary
            )
          } yield
            if allFailed then InboundResult.TempFail("452 4.2.2", "Mailbox full or unavailable")
            else InboundResult.Accepted(id.take(12))
      }
    } yield out
  }

  // 判定に使うローカルのドメインの一覧（毎回 DB を引かないよう同期的に持つ）
  @volatile private var localDomains: Set[String] = Set.empty
  def refreshDomains: IO[Unit] = domains.domains.map(ds => localDomains = ds.filter(_.enabled).map(_.name).toSet)
  private def isLocal(d: String): Boolean = localDomains.contains(d.toLowerCase)

  /** 外から来たメッセージにあると紛らわしいヘッダー（自分が付ける判定結果）を取り除く。 */
  private def stripSpoofable(raw: Array[Byte]): Array[Byte] = {
    val bad = Set("x-spam-score", "x-spam-status", "x-skereru-auth")
    // 自分のホスト名を名乗る Authentication-Results も取り除く（RFC 8601 5章）
    RawHeaders.filter(raw)(h =>
      !(bad(h.name.toLowerCase) ||
        (h.name.equalsIgnoreCase("Authentication-Results") &&
          h.raw.drop(h.raw.indexOf(':') + 1).trim.toLowerCase.startsWith(cfg.hostname.toLowerCase)))
    )
  }

  /** ユーザーのメールボックスに入れる（許可・拒否の差出人、振り分けの規則、自動返信、容量）。 */
  def deliverToUsers(userIds: List[UUID],
                     raw: Array[Byte],
                     blobId: String,
                     spamRes: Option[SpamFilter#Result],
                     mailFrom: String,
                     junk: Boolean,
                     envelopeTo: List[String] = Nil): IO[List[(UUID, Either[String, UUID])]] =
    Telemetry.span(
      "mail.deliver",
      "mail.users" -> userIds.distinct.size.toString
    )(deliverToUsersImpl(userIds, raw, blobId, spamRes, mailFrom, junk, envelopeTo))

  private def deliverToUsersImpl(userIds: List[UUID],
                                 raw: Array[Byte],
                                 blobId: String,
                                 spamRes: Option[SpamFilter#Result],
                                 mailFrom: String,
                                 junk: Boolean,
                                 envelopeTo: List[String]): IO[List[(UUID, Either[String, UUID])]] = {
    val root = Mime.parse(raw)
    val from = root.header("From").flatMap(Address.parseOne).map(_.address.toLowerCase).getOrElse(mailFrom.toLowerCase)
    userIds.distinct.traverse { uid =>
      (for {
        u <- users.find(uid)
        settings <- users.settings(uid)
        qu <- mail.quota(uid, cfg.defaultQuotaBytes)
        (quota, used) = qu
        r <- u match {
          case None => IO.pure(Left("ユーザーがいません"))
          case Some(user) if user.disabled => IO.pure(Left("無効なユーザー"))
          case Some(_) if used + raw.length > quota => IO.pure(Left("容量の上限を超えています"))
          case Some(_) =>
            val listed = (l: List[String]) =>
              l.map(_.trim.toLowerCase).filter(_.nonEmpty).exists(x =>
                from == x || (x.startsWith("@") && from.endsWith(x)) || from.endsWith("@" + x)
              )
            val blocked = listed(settings.blockSenders)
            val allowed = listed(settings.allowSenders)
            for {
              rules <- queue.rules(uid)
              o = Rules.apply(rules, raw, root, envelopeTo)
              isJunk = (junk || blocked) && !allowed && !o.notSpam
              box <- (if isJunk then mail.special(uid, SpecialUse.Junk)
                      else if o.trash then mail.special(uid, SpecialUse.Trash)
                      else
                        o.moveTo.flatTraverse(n => mail.mailboxByName(uid, n)).flatMap(_.fold(mail.special(
                          uid,
                          SpecialUse.Inbox
                        ))(b => IO.pure(Some(b))))
              )
              _ <- IO.whenA(box.isEmpty)(mail.ensureDefaults(uid))
              box2 <- box.fold(mail.special(uid, SpecialUse.Inbox))(b => IO.pure(Some(b)))
              flags = (if o.markRead then List(Flag.Seen) else Nil) ++ (if o.flag then List(Flag.Flagged) else Nil) ++
                (if isJunk then List(Flag.Junk) else Nil)
              meta = Indexer.index(
                raw,
                blobId,
                Instant.now(),
                flags,
                spamRes.map(_.verdict),
                spamRes.map(_.json),
                Some(root)
              )
              m <- mail.insert(uid, box2.get.id, meta)
              _ <- notify(uid)
              // 受信トレイに入った（迷惑メール・既読にする規則のものを除く）新着は、プッシュ通知で知らせる
              inboxNew = !isJunk && !o.markRead && box2.exists(_.specialUse.contains(SpecialUse.Inbox))
              _ <- IO.whenA(inboxNew)(
                onInboxMail(uid, m.id, root).handleErrorWith(e =>
                  Log.report(ErrorCode.NewMailHookFailed, e.toString, e)
                ).start.void
              )
              // stui・Webhook への通知（受信トレイの新着と、規則で「通知先に知らせる」を選んだもの）と、規則の「添付を skawau に保存」
              _ <- IO.whenA(!isJunk && !o.trash && (inboxNew || o.notifyTargets.nonEmpty || o.saveAttachments))(
                onMailStored(uid, m.id, root, box2.fold("")(_.name), inboxNew, o).handleErrorWith(e =>
                  Log.report(ErrorCode.NewMailHookFailed, e.toString, e)
                ).start.void
              )
              _ <- IO.whenA(o.forwards.nonEmpty && !isJunk)(ruleForward(
                uid,
                mailFrom,
                o.forwards,
                raw,
                spamRes.map(_.header)
              ).handleErrorWith(e => Log.report(ErrorCode.ForwardFailed, e.toString, e)))
              // 自動返信は、返信先（エンベロープの差出人）が認証されたメールにだけ返す（偽の差出人への返信＝バックスキャッターを防ぐ）。
              // SPF はエンベロープの差出人のドメインで調べている。DKIM は署名のドメインが返信先と同じ組織のときだけ認める
              authOk = spamRes.forall(r =>
                r.auth.spf.startsWith("pass") || DkimPass.findFirstMatchIn(r.auth.dkim).exists(m =>
                  OrgDomain.of(m.group(1)) == OrgDomain.of(Address.domainOf(mailFrom))
                )
              )
              _ <- IO.whenA(!isJunk && authOk && settings.vacation.enabled)(vacation(
                uid,
                settings,
                root,
                mailFrom
              ).handleErrorWith(e => Log.report(ErrorCode.VacationReplyFailed, e.toString, e)))
              _ <- users.touchContacts(
                uid,
                root.header("From").flatMap(Address.parseOne).toList.map(a => a.address -> a.nameDecoded)
              ).whenA(!isJunk)
            } yield Right(m.id)
        }
      } yield r).handleErrorWith(e => Log.report(ErrorCode.DeliveryFailed, s"$uid: $e", e).as(Left(e.getMessage))).map(
        uid -> _
      )
    }
  }

  /** 振り分けの規則の転送（ユーザーの1時間の送信数の上限に含める。乗っ取られたアカウントの踏み台にさせない）。 */
  private def ruleForward(uid: UUID,
                          mailFrom: String,
                          to: List[String],
                          raw: Array[Byte],
                          auth: Option[String]): IO[Unit] =
    queue.recentRecipients(uid).flatMap { n =>
      if n + to.size > cfg.maxRecipientsPerHour then
        IO(log(s"[deliver] forward skipped: user $uid is over the hourly limit"))
      else
        // エンベロープの差出人は、転送するユーザー自身のアドレスにする（元の差出人のままだと転送先で SPF が通らず、
        // 拒否の通知が元の差出人に届いてしまう＝バックスキャッター）。bounce は空のまま
        if mailFrom.isEmpty then blobs.put(raw).flatMap(id => queue.enqueue(Some(uid), "", to.take(20), id)).void
        else
          domains.sendable(uid).flatMap {
            case own :: _ =>
              signer.arcSeal(raw, Address.domainOf(own), auth, cfg.hostname).flatMap(withArc =>
                blobs.put(withArc).flatMap(id => queue.enqueue(Some(uid), own, to.take(20), id))
              ).void
            case Nil => IO(log(s"[deliver] forward skipped: user $uid has no address"))
          }
    }

  /** 転送（エンベロープの差出人は転送したアドレス。bounce は空のまま）。 */
  private def forward(via: String,
                      mailFrom: String,
                      to: List[String],
                      raw: Array[Byte],
                      auth: Option[String] = None): IO[Unit] = {
    val sender = if mailFrom.isEmpty then "" else via
    signer.arcSeal(raw, Address.domainOf(via), auth, cfg.hostname).flatMap(withArc =>
      blobs.put(withArc).flatMap(id => queue.enqueue(None, sender, to.take(20), id))
    ).void
  }

  private val DkimPass = """^pass \(d=([^)\s]+)\)""".r

  /** 不在の自動返信（同じ相手には4日に1回。自動送信・メーリングリスト・bounce には返さない）。 */
  private def vacation(uid: UUID, s: MailSettingsDto, root: MimePart, mailFrom: String): IO[Unit] = {
    val v = s.vacation
    val today = java.time.LocalDate.now(cfg.zone).toString
    val inPeriod = v.startDate.forall(_ <= today) && v.endDate.forall(_ >= today)
    val auto = root.header("Auto-Submitted").exists(!_.trim.equalsIgnoreCase("no")) ||
      root.header("List-Id").isDefined ||
      root.header("Precedence").exists(p => Set("bulk", "list", "junk")(p.trim.toLowerCase)) || mailFrom.isEmpty ||
      mailFrom.toLowerCase.matches("^(mailer-daemon|postmaster|noreply|no-reply|donotreply)@.*")
    if !inPeriod || auto then IO.unit
    else
      (domains.primaryAddress(uid), users.find(uid)).tupled.flatMap {
        case (Some(me), Some(u)) =>
          val sendIt = if v.contactsOnly then
            users.contacts(uid, mailFrom, 1).map(_.exists(_.address == mailFrom.toLowerCase))
          else IO.pure(true)
          sendIt.flatMap(ok =>
            if !ok then IO.unit
            else
              queue.vacationOnce(uid, mailFrom, 4).flatMap {
                case false => IO.unit
                case true =>
                  val o = Outgoing(
                    MailAddress(Some(u.name), Address.localOf(me), Address.domainOf(me)),
                    List(MailAddress(None, Address.localOf(mailFrom), Address.domainOf(mailFrom))),
                    Nil,
                    if v.subject.trim.nonEmpty then v.subject else "不在のお知らせ",
                    v.body,
                    None,
                    Nil,
                    root.header("Message-ID"),
                    root.header("Message-ID").toList,
                    List("Auto-Submitted" -> "auto-replied", "Precedence" -> "bulk")
                  )
                  val raw =
                    Compose.crlf(Compose.build(o, Compose.newMessageId(Address.domainOf(me)), Instant.now(), cfg.zone))
                  signer.sign(raw).flatMap(signed =>
                    blobs.put(signed).flatMap(id => queue.enqueue(Some(uid), "", List(mailFrom), id))
                  ).void
              }
          )
        case _ => IO.unit
      }
  }

  /**
   * 認証したユーザーが送る（SMTP の submission・画面・API）。From は自分のアドレスに限る。
   * 戻り値は (このサーバーの宛先に配った数, 外への送信待ちにした数)。
   */
  def submit(user: User,
             mailFrom: String,
             rcpts: List[String],
             raw0: Array[Byte],
             via: String,
             ip: Option[String]): IO[Either[String, (Int, Int)]] =
    Telemetry.span(
      "mail.submit",
      "mail.via" -> via,
      "mail.rcpts" -> rcpts.size.toString
    )(submitImpl(user, mailFrom, rcpts, raw0, via, ip))
      .flatTap(r =>
        Metrics.count(
          "skereru.mail.submitted",
          "via" -> Delivery.viaLabel(via),
          "result" -> (if r.isRight then "accepted" else "refused")
        )
      )

  private def submitImpl(user: User,
                         mailFrom: String,
                         rcpts: List[String],
                         raw0: Array[Byte],
                         via: String,
                         ip: Option[String]): IO[Either[String, (Int, Int)]] = {
    val root = Mime.parse(raw0)
    val fromAddrs = root.header("From").toList.flatMap(Address.parseList).map(_.address.toLowerCase)
    for {
      sendable <- domains.sendable(user.id).map(_.toSet)
      recent <- queue.recentRecipients(user.id)
      res <-
        (
          if fromAddrs.isEmpty then IO.pure(Left("From がありません"))
          else if root.headerAll("From").size > 1 || root.headerAll("Sender").size > 1 then
            IO.pure(Left("From・Sender は1つにしてください"))
          else if root.header("Sender").flatMap(Address.parseOne).exists(a => !sendable.contains(a.address.toLowerCase))
          then
            IO.pure(Left("Sender のアドレスは使えません。自分のアドレスにしてください"))
          else if !fromAddrs.forall(sendable.contains) then
            IO.pure(Left(s"From のアドレス（${fromAddrs.mkString(", ")}）は使えません。自分のアドレスにしてください"))
          else if mailFrom.nonEmpty && !sendable.contains(mailFrom.toLowerCase) then
            IO.pure(Left(s"MAIL FROM のアドレス（$mailFrom）は使えません"))
          else if rcpts.isEmpty then IO.pure(Left("宛先がありません"))
          else if recent + rcpts.size > cfg.maxRecipientsPerHour then
            IO.pure(Left(s"送信できる宛先の数の上限（1時間に ${cfg.maxRecipientsPerHour}）を超えています"))
          else {
            val envFrom = if mailFrom.nonEmpty then mailFrom.toLowerCase else fromAddrs.head
            // Date・Message-ID がなければ付ける。Bcc は外へ出さない
            val extra =
              (if root.header("Date").isEmpty then s"Date: ${Dates.rfc5322(Instant.now(), cfg.zone)}\r\n" else "") +
                (if root.header("Message-ID").isEmpty then
                   s"Message-ID: ${Compose.newMessageId(Address.domainOf(envFrom))}\r\n"
                 else "")
            val noBcc = removeHeader(Compose.crlf(raw0), "Bcc")
            val withHead =
              (s"Received: by ${cfg.hostname} (skereru) with ${via}; ${Dates.rfc5322(Instant.now(), cfg.zone)}\r\n" +
                extra).getBytes(StandardCharsets.ISO_8859_1) ++ noBcc
            for {
              signed <- signer.sign(withHead)
              id <- blobs.put(signed)
              split <- rcpts.distinct.traverse(r => isLocalDomain(Address.domainOf(r)).map(r -> _))
              (local, remote) = split.partitionMap((r, l) => if l then Left(r) else Right(r))
              targets <- local.traverse(r => resolve(r).map(r -> _))
              missing = targets.collect { case (r, None) => r }
              delivered <- targets.flatMap(_._2.toList).traverse(t =>
                deliverToUsers(t.users, signed, id, None, envFrom, junk = false, envelopeTo = local)
              ).map(_.flatten.count(_._2.isRight))
              fwds = targets.flatMap(_._2.toList.flatMap(_.forwards)).distinct
              queued <- queue.enqueue(Some(user.id), envFrom, (remote ++ fwds).distinct, id)
              // このサーバーの存在しない宛先は、すぐに差出人へ知らせる
              _ <-
                IO.whenA(missing.nonEmpty)(bounceToSender(envFrom, signed, missing.map(_ -> "550 5.1.1 宛先のアドレスがありません")))
              _ <- users.touchContacts(
                user.id,
                (root.header("To").toList ++ root.header("Cc").toList).flatMap(Address.parseList).map(a =>
                  a.address -> a.nameDecoded
                )
              )
              _ <- queue.log("submit", "sent", ip, envFrom, rcpts, signed.length, None, via)
            } yield Right((delivered, queued))
          }
        )
    } yield res
  }

  private def removeHeader(raw: Array[Byte], name: String): Array[Byte] =
    RawHeaders.filter(raw)(!_.name.equalsIgnoreCase(name))

  /** 配送できなかったことを差出人に知らせる（このサーバーのユーザーなら直接、外なら送信待ちに）。 */
  def bounceToSender(sender: String, original: Array[Byte], failed: List[(String, String)]): IO[Unit] =
    if sender.isEmpty then IO.unit
    else {
      val pm = MailAddress(Some("Mail Delivery System"), "MAILER-DAEMON", cfg.hostname)
      val dsn = Compose.bounce(pm, sender, original, failed, cfg.hostname, Instant.now(), cfg.zone)
      blobs.put(dsn).flatMap { id =>
        resolve(sender).flatMap {
          case Some(t) => deliverToUsers(t.users, dsn, id, None, "", junk = false).void
          case None => isLocalDomain(Address.domainOf(sender)).flatMap(l =>
              if l then IO.unit else queue.enqueue(None, "", List(sender), id).void
            )
        }
      }
    }
}

object Delivery {

  /** DMARC の集計レポートを受け取るアドレスのローカル部（dmarc-reports@<ドメイン>）。 */
  val DmarcReportsLocal = "dmarc-reports"

  /** これより多くの Received があるメールはループとみなす（RFC 5321 6.3 の目安は 100）。 */
  val MaxHops = 50

  /** 認証の結果（"pass (d=…)" など）の最初の語（メトリクスの属性）。 */
  private[mail] def word(s: String): String = s.trim.takeWhile(c => !c.isWhitespace && c != '(').toLowerCase match {
    case "" => "none"
    case w => w
  }

  /** 迷惑メールとして拒否した主な理由（メトリクスの属性）。 */
  private[mail] def rejectReason(v: SpamVerdict): String = {
    val rules = v.hits.map(_.rule).toSet
    if rules("VIRUS") then "virus"
    else if rules("DMARC_REJECT") || rules("DMARC_QUARANTINE") then "dmarc"
    else if rules("SPOOFED_LOCAL_DOMAIN") then "spoofing"
    else if rules("SPF_FAIL") then "spf"
    else if rules("DKIM_INVALID") then "dkim"
    else if rules("RCVD_IN_DNSBL") then "dnsbl"
    else "spam"
  }

  /** 送信の経路（ESMTPSA・画面など）を、メトリクスの属性の少ない値にまとめる。 */
  private[mail] def viaLabel(via: String): String =
    via.toUpperCase match {
      case v if v.startsWith("ESMTP") => "smtp"
      case "HTTPS" => "web"
      case "HTTPSA" => "api"
      case _ => "other"
    }
}
