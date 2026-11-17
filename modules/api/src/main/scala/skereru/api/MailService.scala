package skereru.api

import cats.effect.*
import cats.syntax.all.*
import skereru.core.*
import skereru.db.*
import skereru.mail.{Delivery, Indexer, MailConfig}
import skereru.shared.*
import skereru.storage.Blobs

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.UUID

/** 画面・API・MCP からのメールの操作（一覧・表示・整理・送信・下書き）。 */
final class MailService(mail: MailStore, users: UserStore, domains: DomainStore, queue: QueueStore, blobs: Blobs, delivery: Delivery,
    mcfg: MailConfig, changed: UUID => IO[Unit], schedules: ScheduleStore):

  private def addrs(s: String): List[AddrDto] =
    if s.trim.isEmpty then Nil else Address.parseList(s).map(a => AddrDto(a.nameDecoded, a.address))

  def summary(m: MsgMeta, threadCount: Int = 1): MessageSummaryDto =
    MessageSummaryDto(m.id, m.mailboxId, m.threadId, m.subject, m.fromAddr.map(a => AddrDto(m.fromName, a)), addrs(m.toAddrs),
      m.sentAt.filter(_.isBefore(m.internalDate.plusSeconds(86400))).getOrElse(m.internalDate), m.preview,
      m.flags.contains(Flag.Seen), m.flags.contains(Flag.Flagged), m.flags.contains(Flag.Answered), m.flags.contains(Flag.Draft),
      m.hasAttachments, m.size, threadCount, m.spamScore)

  private val order = SpecialUse.all.zipWithIndex.toMap

  def mailboxes(userId: UUID): IO[List[MailboxDto]] =
    mail.ensureDefaults(userId) *> mail.mailboxDtos(userId).map(_.sortBy(b => (b.specialUse.flatMap(order.get).getOrElse(100), b.name.toLowerCase)))

  def list(s: AuthSession, mailboxId: Option[UUID], q: String, offset: Int, limit: Int): Res[MessageListDto] =
    for
      settings <- users.settings(s.user.id)
      box <- mailboxId.traverse(id => mail.mailbox(id))
      r <- box match
        case Some(None)                              => IO.pure(Left(Fail.NotFound("メールボックスが見つかりません")))
        case Some(Some(b)) if b.userId != s.user.id => IO.pure(Left(Fail.NotFound("メールボックスが見つかりません")))
        case _ =>
          // 下書き・送信済みはスレッドにまとめない
          val conv = settings.conversationView && !box.flatten.exists(b => b.specialUse.exists(Set(SpecialUse.Drafts, SpecialUse.Sent)))
          mail.list(s.user.id, mailboxId, MailQuery.parse(q), conv, offset, limit, Op.zone(s.user.timezone))
            .map((rows, total) => Right(MessageListDto(rows.map((m, n) => summary(m, n)), total, offset)))
    yield r

  def load(userId: UUID, id: UUID): Res[(MsgMeta, Array[Byte])] =
    mail.find(userId, id).flatMap {
      case None => IO.pure(Left(Fail.NotFound("メッセージが見つかりません")))
      case Some(m) => blobs.get(m.blobId).map(_.toRight(Fail.NotFound("メッセージの本体が見つかりません")).map(m -> _))
    }

  private def partPath(p: List[Int]): String = p.mkString(".")

  /** HTML の部品を無害化する（cid: の画像はこのサーバーの URL に）。 */
  private def sanitized(m: MsgMeta, raw: Array[Byte], leaves: List[(List[Int], MimePart)], images: Boolean): Option[(String, Boolean)] =
    leaves.find((_, p) => p.mimeType == "text/html" && !p.isAttachment).map { (_, p) =>
      val cid = (c: String) => leaves.find(_._2.contentId.contains(c)).map((path, _) => s"/api/messages/${m.id}/parts/${partPath(path)}")
      Html.sanitize(Mime.text(raw, p), cid, images)
    }

  /** HTML の本文（無害化済み）。images=false なら外部の画像は読み込まない。 */
  /** 戻り値は（HTML、外部の画像があるか、外部の画像を許したか）。 */
  def html(userId: UUID, id: UUID, images: Boolean): Res[Option[(String, Boolean, Boolean)]] =
    IO.both(load(userId, id), users.settings(userId)).map { (r, settings) =>
      val allow = images || settings.alwaysShowImages
      r.map((m, raw) => sanitized(m, raw, Mime.parse(raw).leaves(), allow).map((h, remote) => (h, remote, allow)))
    }

  def detail(userId: UUID, id: UUID): Res[MessageDetailDto] =
    IO.both(load(userId, id), users.settings(userId)).flatMap {
      case (Left(f), _) => IO.pure(Left(f))
      case (Right((m, raw)), settings) =>
        mail.mailbox(m.mailboxId).map { box =>
          val root = Mime.parse(raw)
          val leaves = root.leaves()
          val h = sanitized(m, raw, leaves, settings.alwaysShowImages)
          val htmlRaw = leaves.find((_, p) => p.mimeType == "text/html" && !p.isAttachment).map((_, p) => Mime.text(raw, p)).getOrElse("")
          val text = leaves.find((_, p) => p.mimeType == "text/plain" && !p.isAttachment).map((_, p) => Mime.text(raw, p))
            .getOrElse(if htmlRaw.nonEmpty then Html.toText(htmlRaw) else "")
          val atts = leaves.filter((_, p) => p.isAttachment || (p.contentId.isDefined && p.mainType == "image")).map { (path, p) =>
            val size = if p.encoding == "base64" then p.size * 3 / 4 else p.size
            val inline = p.contentId.exists(c => htmlRaw.contains(s"cid:$c"))
            AttachmentDto(partPath(path), p.filename.getOrElse(s"無題.${p.subType}"), p.mimeType, size, inline, p.contentId)
          }
          def hl(n: String) = root.header(n).toList.flatMap(Address.parseList).map(a => AddrDto(a.nameDecoded, a.address))
          Right(MessageDetailDto(
            summary(m), hl("Cc"), hl("Bcc"), hl("Reply-To"), m.messageId,
            root.header("References").toList.flatMap(r => "<[^>]+>".r.findAllIn(r).toList),
            h.map(_._1), text, h.exists(_._2), atts, m.spamReport,
            m.authResults.flatMap(_.as[AuthResultDto].toOption), root.header("List-Unsubscribe"), box.map(_.name).getOrElse(""),
            // 会議の招待（iCalendar）を含むか（画面が招待のカードを出す）
            hasInvite = root.leaves().exists((_, p) => p.mimeType == "text/calendar" || p.mimeType == "application/ics" || p.filename.exists(_.toLowerCase.endsWith(".ics")))
          ))
        }
    }

  /** スレッド（開いたメッセージの会話）。markRead なら未読を既読にする。 */
  def thread(userId: UUID, id: UUID, markRead: Boolean): Res[ThreadDto] =
    mail.find(userId, id).flatMap {
      case None => IO.pure(Left(Fail.NotFound("メッセージが見つかりません")))
      case Some(m) =>
        for
          box <- mail.mailbox(m.mailboxId)
          inTrash = box.exists(_.specialUse.exists(u => u == SpecialUse.Trash || u == SpecialUse.Junk))
          ms0 <- mail.thread(userId, m.threadId, inTrash)
          ms1 = if ms0.exists(_.id == m.id) then ms0 else ms0 :+ m
          // 同じメッセージの複製（受信トレイと送信済みの控えなど）は1つにする（開いたもの、なければ最初のもの）
          ms = ms1.filter(x => x.messageId.isEmpty || x.id == m.id || !ms1.exists(y => y.messageId == x.messageId && (y.id == m.id || ms1.indexOf(y) < ms1.indexOf(x))))
          details <- ms.traverse(x => detail(userId, x.id): IO[Either[Fail, MessageDetailDto]]).map(_.flatMap(_.toOption))
          unread = ms.filterNot(_.flags.contains(Flag.Seen)).map(_.id)
          _ <- IO.whenA(markRead && unread.nonEmpty)(mail.setFlags(userId, unread, List(Flag.Seen), Nil) *> changed(userId))
        yield Right(ThreadDto(m.threadId, m.subject, details.map(d => if unread.contains(d.summary.id) && markRead then d.copy(summary = d.summary.copy(seen = true)) else d)))
    }

  def raw(userId: UUID, id: UUID): Res[(MsgMeta, Array[Byte])] = load(userId, id)

  /** 添付ファイル（部品番号 1.2 など）の中身。 */
  def part(userId: UUID, id: UUID, path: String): Res[(String, String, Array[Byte])] =
    load(userId, id).map(_.flatMap { (_, raw) =>
      val nums = path.split('.').toList.flatMap(_.toIntOption)
      val root = Mime.parse(raw)
      root.leaves().find(_._1 == nums).map(_._2).toRight(Fail.NotFound("添付ファイルが見つかりません")).map { p =>
        (p.filename.getOrElse("attachment"), p.mimeType, Mime.content(raw, p))
      }
    })

  /** まとめての変更（既読・スター・移動・迷惑メールの報告・削除）。 */
  def update(userId: UUID, r: UpdateMessagesReq): Res[Int] =
    val ids = r.ids.distinct.take(1000)
    for
      metas <- mail.findMany(userId, ids)
      boxes <- mail.mailboxes(userId).map(_.map(b => b.id -> b).toMap)
      found = metas.map(_.id)
      _ <- (r.seen, r.flagged) match
        case (None, None) => IO.pure(0)
        case _ =>
          val add = r.seen.filter(identity).map(_ => Flag.Seen).toList ++ r.flagged.filter(identity).map(_ => Flag.Flagged).toList
          val rem = r.seen.filterNot(identity).map(_ => Flag.Seen).toList ++ r.flagged.filterNot(identity).map(_ => Flag.Flagged).toList
          mail.setFlags(userId, found, add, rem)
      _ <- r.spam match
        case None => IO.pure(0)
        case Some(spam) =>
          // 学習してから、迷惑メール（または受信トレイ）へ移す
          metas.take(200).traverse_(m => blobs.get(m.blobId).flatMap(_.traverse_(raw => queue.train(userId, Bayes.tokens(raw, Mime.parse(raw)), spam, undo = false)))) *>
            mail.special(userId, if spam then SpecialUse.Junk else SpecialUse.Inbox).flatMap(_.traverse(b =>
              mail.setFlags(userId, found, List(if spam then Flag.Junk else Flag.NotJunk), List(if spam then Flag.NotJunk else Flag.Junk)) *>
                mail.moveMessages(userId, found, b.id)).map(_.getOrElse(0)))
      _ <- r.moveTo match
        case None => IO.pure(0)
        case Some(to) => if boxes.get(to).isEmpty then IO.pure(0) else mail.moveMessages(userId, found, to)
      _ <- r.delete match
        case Some(true) =>
          // ゴミ箱・迷惑メールの中のものは完全に消し、ほかはゴミ箱へ
          val (perm, soft) = metas.partition(m => boxes.get(m.mailboxId).exists(_.specialUse.exists(u => u == SpecialUse.Trash || u == SpecialUse.Junk)))
          for
            a <- mail.deleteMessages(userId, perm.map(_.id))
            t <- mail.special(userId, SpecialUse.Trash)
            b <- t.traverse(tb => mail.moveMessages(userId, soft.map(_.id), tb.id)).map(_.getOrElse(0))
          yield a + b
        case _ => IO.pure(0)
      _ <- changed(userId)
    yield Right(found.size)

  def emptyMailbox(userId: UUID, boxId: UUID): Res[Int] =
    mail.mailbox(boxId).flatMap {
      case Some(b) if b.userId == userId && b.specialUse.exists(u => u == SpecialUse.Trash || u == SpecialUse.Junk) =>
        mail.emptyMailbox(boxId).flatTap(_ => changed(userId)).map(Right(_))
      case Some(b) if b.userId == userId => IO.pure(Left(Fail.Invalid("空にできるのはゴミ箱と迷惑メールだけです")))
      case _ => IO.pure(Left(Fail.NotFound("メールボックスが見つかりません")))
    }

  // ---------------------------------------------------------------- 書く

  /** 保存の容量（使っている分 + まだ使っていないアップロード + extra が上限以内か）。HTTP からの保存も、受信・IMAP と同じく容量を守る。 */
  /** 下書き・送信では、添付が本文に入るのでアップロードの分は数えない（二重に数えない）。 */
  private def withinQuota(userId: UUID, extra: Long, countUploads: Boolean = false): IO[Boolean] =
    (mail.quota(userId, mcfg.defaultQuotaBytes), if countUploads then mail.pendingUploads(userId) else IO.pure((0L, 0L))).mapN {
      case ((quota, used), (_, pending)) => used + pending + extra <= quota
    }

  private val OverQuota = Fail.TooLarge("保存できる容量の上限を超えています。不要なメールを削除してください")
  private val MaxPendingUploads = 100

  def upload(userId: UUID, filename: String, ct: String, bytes: Array[Byte]): Res[UploadDto] =
    mail.pendingUploads(userId).flatMap { (count, _) =>
      if count >= MaxPendingUploads then IO.pure(Left(Fail.TooLarge("添付ファイルが多すぎます。しばらくしてからやり直してください")))
      else withinQuota(userId, bytes.length.toLong, countUploads = true).flatMap {
        case false => IO.pure(Left(OverQuota))
        case true  => blobs.put(bytes).flatMap(id => mail.createUpload(userId, filename, ct, bytes.length.toLong, id)).map(Right(_))
      }
    }

  /** 返信・全員に返信・転送の初期値。 */
  def template(s: AuthSession, id: UUID, mode: String): Res[ComposeTemplateDto] =
    IO.both(detail(s.user.id, id), domains.sendable(s.user.id)).map { (d, mine) =>
      d.map { m =>
        val myAddrs = mine.toSet
        val toMe = (m.summary.to ++ m.cc).map(_.address.toLowerCase).find(myAddrs.contains)
        val from = toMe.orElse(mine.headOption).getOrElse("")
        val original = m.replyTo.headOption.orElse(m.summary.from).toList.map(fmt)
        val date = m.summary.date.toString
        val who = m.summary.from.map(fmt).getOrElse("")
        mode match
          case "forward" =>
            val head = s"---------- 転送メッセージ ----------\nFrom: $who\nDate: $date\nSubject: ${m.summary.subject}\nTo: ${m.summary.to.map(fmt).mkString(", ")}\n\n"
            ComposeTemplateDto(from, Nil, Nil, Compose.forwardSubject(m.summary.subject), head + m.text,
              "<p>---------- 転送メッセージ ----------<br>" + Html.fromText(head.trim) + "</p>" + m.html.getOrElse(Html.fromText(m.text)),
              m.attachments.filterNot(_.inline))
          case _ =>
            val all = mode == "replyAll"
            val others = if all then (m.summary.to ++ m.cc).filterNot(a => myAddrs.contains(a.address.toLowerCase)).map(fmt) else Nil
            val to = if m.summary.from.exists(f => myAddrs.contains(f.address.toLowerCase)) then m.summary.to.map(fmt) else original
            val quoteHead = s"$date $who:"
            ComposeTemplateDto(from, to.distinct, others.filterNot(to.contains).distinct, Compose.replySubject(m.summary.subject),
              s"\n\n$quoteHead\n" + Compose.quote(m.text),
              s"<p></p><p>${Html.escape(quoteHead)}</p><blockquote>${m.html.getOrElse(Html.fromText(m.text))}</blockquote>", Nil)
      }
    }

  private def fmt(a: AddrDto): String = MailText.quoted(a.name, a.address)

  /** 宛先の文字列（"名前 <addr>"・カンマ区切り）をアドレスにする。 */
  private def parseRcpts(xs: List[String]): Either[Fail, List[MailAddress]] =
    val all = xs.flatMap(x => Address.parseList(x))
    all.find(a => !Address.valid(Address.toAscii(a.address))) match
      case Some(bad) => Left(Fail.Invalid(s"宛先のアドレスが正しくありません: ${bad.address}"))
      case None => Right(all.map(a => a.copy(local = a.local, domain = Address.domainOf(Address.toAscii(a.address)))))

  /** 送るメッセージを組み立てる（下書きの保存にも使う）。 */
  private def build(s: AuthSession, r: ComposeReq): Res[(Array[Byte], List[MailAddress], List[MailAddress], List[MailAddress], MailAddress, Option[MsgMeta])] =
    (domains.sendable(s.user.id), mail.uploads(s.user.id, r.uploadIds), r.inReplyTo.orElse(r.forwardOf).flatTraverse(id => mail.find(s.user.id, id))).tupled.flatMap {
      (mine, ups, orig) =>
        val from = r.from.trim.toLowerCase
        (for
          _ <- Either.cond(mine.contains(from), (), Fail.Forbidden(s"差出人のアドレス（$from）は使えません"))
          to <- parseRcpts(r.to)
          cc <- parseRcpts(r.cc)
          bcc <- parseRcpts(r.bcc)
          _ <- Either.cond((to ++ cc ++ bcc).size <= mcfg.maxRecipients, (), Fail.Invalid(s"宛先は ${mcfg.maxRecipients} 件までです"))
          _ <- Either.cond(r.subject.length <= 1000, (), Fail.Invalid("件名が長すぎます"))
        yield (to, cc, bcc)) match
          case Left(f) => IO.pure(Left(f))
          case Right((to, cc, bcc)) =>
            for
              upFiles <- ups.traverse(u => blobs.get(u.blobId).map(_.map(b => Attachment(u.filename, u.contentType, b))))
              fwdFiles <- r.forwardOf.flatTraverse(id => raw(s.user.id, id).map(_.toOption)).map(_.toList.flatMap { (_, raw0) =>
                Mime.parse(raw0).leaves().map(_._2).filter(_.isAttachment).map(p => Attachment(p.filename.getOrElse("attachment"), p.mimeType, Mime.content(raw0, p)))
              })
              me <- users.find(s.user.id)
              fromAddr = MailAddress(me.map(_.name), Address.localOf(from), Address.domainOf(from))
              refs = orig.filter(_ => r.inReplyTo.isDefined).toList.flatMap(o => o.messageId.toList)
              origRefs <- orig.filter(_ => r.inReplyTo.isDefined).flatTraverse(o => blobs.get(o.blobId).map(_.flatMap(b => Mime.parse(b).header("References"))))
              allRefs = (origRefs.toList.flatMap(x => "<[^>]+>".r.findAllIn(x).toList) ++ refs).distinct.takeRight(20)
              html = r.html.map(h => Html.sanitize(h, _ => None, allowRemote = true)._1)
              total = (upFiles.flatten ++ fwdFiles).map(_.data.length.toLong).sum
              res <-
                if total > mcfg.maxMessageBytes * 3 / 4 then IO.pure(Left(Fail.TooLarge("添付ファイルが大きすぎます")))
                else
                  val o = Outgoing(fromAddr, to, cc, r.subject, r.text, html, upFiles.flatten ++ fwdFiles, refs.headOption, allRefs)
                  val bytes = Compose.crlf(Compose.build(o, Compose.newMessageId(Address.domainOf(from)), Instant.now(), Op.zone(s.user.timezone)))
                  IO.pure(Right((bytes, to, cc, bcc, fromAddr, orig)))
            yield res
    }

  /** Bcc を付けたもの（送信済み・下書きに残す控え）。 */
  private def withBcc(raw: Array[Byte], bcc: List[MailAddress]): Array[Byte] =
    if bcc.isEmpty then raw
    else (s"Bcc: ${bcc.map(_.address).mkString(", ")}\r\n").getBytes(StandardCharsets.ISO_8859_1) ++ raw

  def send(s: AuthSession, r: ComposeReq, ip: String): Res[SentDto] =
    build(s, r).flatMap {
      case Left(f) => IO.pure(Left(f))
      case Right((bytes, to, cc, bcc, from, orig)) =>
        val rcpts = (to ++ cc ++ bcc).map(_.address.toLowerCase).distinct
        if rcpts.isEmpty then IO.pure(Left(Fail.Invalid("宛先を入力してください")))
        else withinQuota(s.user.id, bytes.length.toLong).flatMap {
          case false => IO.pure(Left(OverQuota))   // 送信済みの控えを残せない
          case true =>
          delivery.submit(s.user, from.address, rcpts, bytes, if s.isSession then "HTTPS" else "HTTPSA", Some(ip)).flatMap {
            case Left(e) => IO.pure(Left(Fail.Forbidden(e)))
            case Right((delivered, queued)) =>
              val copy = withBcc(bytes, bcc)
              for
                sent <- mail.special(s.user.id, SpecialUse.Sent)
                id <- blobs.put(copy)
                m <- mail.insert(s.user.id, sent.get.id, Indexer.index(copy, id, Instant.now(), List(Flag.Seen), None, None))
                _ <- orig.traverse_(o => mail.setFlags(s.user.id, List(o.id), List(if r.forwardOf.isDefined then Flag.Forwarded else Flag.Answered), Nil))
                _ <- r.draftId.traverse_(d => mail.find(s.user.id, d).flatMap(_.filter(_.flags.contains(Flag.Draft)).traverse_(x => mail.deleteMessages(s.user.id, List(x.id)))))
                _ <- changed(s.user.id)
              yield Right(SentDto(m.id, queued, delivered))
          }
        }
    }

  // ---------------------------------------------------------------- 予約送信・送信の取り消し

  /** 送信を予約する（sendAt がなければ、設定の取り消しの猶予の後）。中身は今確かめて、宛先の誤りなどはすぐに返す。 */
  def schedule(s: AuthSession, r: ComposeReq, sendAt: Option[Instant], ip: String): Res[ScheduledDto] =
    IO.realTimeInstant.flatMap { now =>
      if sendAt.exists(_.isAfter(now.plusSeconds(366L * 24 * 3600))) then IO.pure(Left(Fail.Invalid("予約できるのは1年先までです")))
      else if sendAt.exists(_.isBefore(now.minusSeconds(60))) then IO.pure(Left(Fail.Invalid("過去の日時には予約できません")))
      else build(s, r).flatMap {
        case Left(f) => IO.pure(Left(f))
        case Right((_, to, cc, bcc, _, _)) if (to ++ cc ++ bcc).isEmpty => IO.pure(Left(Fail.Invalid("宛先を入力してください")))
        case Right(_) =>
          users.settings(s.user.id).flatMap { st =>
            val at = sendAt.getOrElse(now.plusSeconds(math.max(0, math.min(st.undoSendSeconds, 60)).toLong))
            schedules.create(s.user.id, at, r, Some(ip)).map(Right(_))
          }
      }
    }

  /** 時刻が来た予約を送る（定期処理から呼ぶ）。 */
  def sendDue: IO[Unit] =
    schedules.claimDue().flatMap(_.traverse_ { d =>
      users.find(d.userId).flatMap {
        case Some(u) if !u.disabled =>
          send(AuthSession(u, Nil, isAdmin = false), d.req, d.ip.getOrElse("-")).attempt.flatMap {
            case Right(Right(_)) => schedules.finish(d.id, None)
            case Right(Left(f))  => schedules.finish(d.id, Some(f.message))
            case Left(e)         => schedules.finish(d.id, Some(Option(e.getMessage).getOrElse(e.toString)))
          } *> changed(u.id)
        case _ => schedules.finish(d.id, Some("ユーザーが無効です"))
      }
    }) *> schedules.cleanup()

  // ---------------------------------------------------------------- スヌーズ

  def snooze(userId: UUID, ids: List[UUID], until: Option[Instant]): Res[Int] =
    IO.realTimeInstant.flatMap { now =>
      if until.exists(!_.isAfter(now)) then IO.pure(Left(Fail.Invalid("スヌーズは今より後の日時にしてください")))
      else mail.snooze(userId, ids.take(1000), until).flatTap(_ => changed(userId)).map(Right(_))
    }

  /** 時刻が来たスヌーズを戻す（定期処理から呼ぶ）。 */
  def wakeSnoozed: IO[Unit] = mail.wakeSnoozed().flatMap(_.traverse_(changed))

  def saveDraft(s: AuthSession, r: ComposeReq): Res[DraftSavedDto] =
    build(s, r.copy(to = r.to.filter(_.trim.nonEmpty), cc = r.cc.filter(_.trim.nonEmpty), bcc = r.bcc.filter(_.trim.nonEmpty))).flatMap {
      case Left(f) => IO.pure(Left(f))
      case Right((bytes, _, _, bcc, _, _)) =>
        val copy = withBcc(bytes, bcc)
        withinQuota(s.user.id, copy.length.toLong).ifM(
        for
          drafts <- mail.special(s.user.id, SpecialUse.Drafts)
          id <- blobs.put(copy)
          m <- mail.insert(s.user.id, drafts.get.id, Indexer.index(copy, id, Instant.now(), List(Flag.Seen, Flag.Draft), None, None))
          _ <- r.draftId.traverse_(d => mail.find(s.user.id, d).flatMap(_.filter(_.flags.contains(Flag.Draft)).traverse_(x => mail.deleteMessages(s.user.id, List(x.id)))))
          _ <- changed(s.user.id)
        yield Right(DraftSavedDto(m.id)),
        IO.pure(Left(OverQuota)))
    }

  /** 下書きを開く（作成画面に戻す）。 */
  def draft(s: AuthSession, id: UUID): Res[ComposeReq] =
    detail(s.user.id, id).map(_.map { d =>
      ComposeReq(d.summary.from.map(_.address).getOrElse(""), d.summary.to.map(fmt), d.cc.map(fmt), d.bcc.map(fmt), d.summary.subject, d.text,
        d.html, Nil, None, None, Some(id))
    })
