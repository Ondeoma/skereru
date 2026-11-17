package skereru.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import skereru.core.{MailQuery, SearchKey, SeqSet}
import skereru.shared.*

import java.time.ZoneId
import java.util.UUID

/** メールボックスとメッセージ。UID・MODSEQ はメールボックスの行を更新して払い出す（同時に書いても重ならない）。 */
final class MailStore(xa: Transactor[IO]) extends TxStore(xa):
  /** 検索の語（種類ごと）の数の上限。 */
  private val MaxTerms = 10

  private val mcols = fr"id, user_id, name, special_use, uidvalidity, uidnext, highest_modseq, subscribed"
  private val cols = fr"""m.id, m.user_id, m.mailbox_id, m.uid, m.modseq, m.blob_id, m.size, m.internal_date, m.flags, m.subject, m.from_addr, m.from_name,
    m.to_addrs, m.cc_addrs, m.sent_at, m.message_id, m.in_reply_to, m.thread_id, m.has_attachments, m.preview, m.spam_score::float8, m.spam_report, m.auth_results"""

  // ---------------------------------------------------------------- メールボックス

  /** 新しいユーザーの既定のメールボックスを作る（あれば何もしない）。 */
  def ensureDefaults(userId: UUID): IO[Unit] = tx(ensureDefaultsC(userId))
  def ensureDefaultsC(userId: UUID): ConnectionIO[Unit] =
    SpecialUse.all.traverse_ { u =>
      sql"""insert into mailbox (id, user_id, name, special_use, uidvalidity) values (${UUID.randomUUID()}, $userId, ${SpecialUse.defaultName(u)}, $u, ${uidValidity()})
            on conflict do nothing""".update.run
    }

  private def uidValidity(): Long = (System.currentTimeMillis() / 1000) & 0x7fffffffL

  def mailboxes(userId: UUID): IO[List[Mailbox]] = tx(mailboxesC(userId))
  def mailboxesC(userId: UUID): ConnectionIO[List[Mailbox]] = (fr"select" ++ mcols ++ fr"from mailbox where user_id = $userId order by name").query[Mailbox].to[List]

  def mailbox(id: UUID): IO[Option[Mailbox]] = tx((fr"select" ++ mcols ++ fr"from mailbox where id = $id").query[Mailbox].option)
  def mailboxByName(userId: UUID, name: String): IO[Option[Mailbox]] = tx(mailboxByNameC(userId, name))
  def mailboxByNameC(userId: UUID, name: String): ConnectionIO[Option[Mailbox]] =
    (fr"select" ++ mcols ++ fr"from mailbox where user_id = $userId and name = $name").query[Mailbox].option
  def special(userId: UUID, use: String): IO[Option[Mailbox]] = tx(specialC(userId, use))
  def specialC(userId: UUID, use: String): ConnectionIO[Option[Mailbox]] =
    (fr"select" ++ mcols ++ fr"from mailbox where user_id = $userId and special_use = $use").query[Mailbox].option

  /** 件数と未読の件数（画面の一覧用）。 */
  def mailboxDtos(userId: UUID): IO[List[MailboxDto]] = tx(
    sql"""select b.id, b.name, b.special_use, coalesce(c.total, 0), coalesce(c.unread, 0), b.subscribed
          from mailbox b left join (
            select mailbox_id, count(*)::int as total, count(*) filter (where not ('\Seen' = any(flags)))::int as unread
            from message where user_id = $userId group by mailbox_id) c on c.mailbox_id = b.id
          where b.user_id = $userId""".query[MailboxDto].to[List]
  )

  def createMailbox(userId: UUID, name: String): IO[Either[StoreError, Mailbox]] = tx(
    mailboxByNameC(userId, name).flatMap {
      case Some(_) => fail(StoreError.Conflict(s"メールボックス「$name」はすでにあります"))
      case None =>
        sql"select count(*) from mailbox where user_id = $userId".query[Int].unique.flatMap { n =>
          if n >= MailStore.MaxMailboxes then fail(StoreError.Invalid(s"メールボックスは ${MailStore.MaxMailboxes} 個までです"))
          else (fr"insert into mailbox (id, user_id, name, uidvalidity) values (${UUID.randomUUID()}, $userId, $name, ${uidValidity()}) returning" ++ mcols)
          .query[Mailbox].unique.map(Right(_))
        }
    }
  )

  /** 名前を変える（子のメールボックス "名前/…" も変える。INBOX は変えられない）。 */
  def renameMailbox(userId: UUID, from: String, to: String): IO[Either[StoreError, Unit]] = tx(
    mailboxByNameC(userId, from).flatMap {
      case None => fail(StoreError.NotFound(s"メールボックス「$from」"))
      case Some(b) if b.specialUse.contains(SpecialUse.Inbox) => fail(StoreError.Invalid("受信トレイの名前は変えられません"))
      case Some(_) =>
        mailboxByNameC(userId, to).flatMap {
          case Some(_) => fail(StoreError.Conflict(s"メールボックス「$to」はすでにあります"))
          case None =>
            val prefix = from + "/"
            sql"""update mailbox set name = $to || substr(name, ${from.length + 1}) where user_id = $userId and (name = $from or starts_with(name, $prefix))"""
              .update.run.as(Right(()))
        }
    }
  )

  def deleteMailbox(userId: UUID, name: String): IO[Either[StoreError, Unit]] = tx(
    mailboxByNameC(userId, name).flatMap {
      case None => fail(StoreError.NotFound(s"メールボックス「$name」"))
      case Some(b) if b.specialUse.isDefined => fail(StoreError.Invalid("この特別なメールボックスは削除できません"))
      case Some(b) =>
        sql"select count(*) from mailbox where user_id = $userId and starts_with(name, ${name + "/"})".query[Int].unique.flatMap { kids =>
          if kids > 0 then fail(StoreError.Invalid("中にメールボックスがあるので削除できません"))
          else sql"delete from mailbox where id = ${b.id}".update.run.as(Right(()))
        }
    }
  )

  def subscribe(userId: UUID, name: String, on: Boolean): IO[Int] = tx(
    sql"update mailbox set subscribed = $on where user_id = $userId and name = $name".update.run
  )

  /** 変化を知るための値（どれかのメールボックスが変わると変わる）。 */
  def changeToken(userId: UUID): IO[Long] = tx(
    sql"select coalesce(sum(highest_modseq), 0)::int8 * 1000 + count(*) from mailbox where user_id = $userId".query[Long].unique
  )

  /** メールボックスの UID と MODSEQ を払い出す（n 個）。戻り値は最初の UID と MODSEQ。 */
  private def allocate(mailboxId: UUID, n: Int): ConnectionIO[(Long, Long)] =
    sql"""update mailbox set uidnext = uidnext + $n, highest_modseq = highest_modseq + 1 where id = $mailboxId returning uidnext - $n, highest_modseq"""
      .query[(Long, Long)].unique

  private def bumpModseq(mailboxId: UUID): ConnectionIO[Long] =
    sql"update mailbox set highest_modseq = highest_modseq + 1 where id = $mailboxId returning highest_modseq".query[Long].unique

  // ---------------------------------------------------------------- 本体（blob）

  /** 本体を保存する（同じ内容は1つにまとめる。data が None なら外部のストレージに置いたもの）。 */
  def putBlob(id: String, size: Long, data: Option[Array[Byte]]): IO[Unit] = tx(
    sql"""insert into blob (id, size, data) values ($id, $size, $data) on conflict (id) do update set created_at = now()""".update.run.void
  )

  def blobData(id: String): IO[Option[Option[Array[Byte]]]] = tx(sql"select data from blob where id = $id".query[Option[Array[Byte]]].option)

  /** どこからも使われていない古い本体を消す（消した ID を返す。外部のストレージからも消すため）。 */
  def purgeBlobs(limit: Int): IO[List[String]] = tx(
    sql"""delete from blob b where b.id in (
            select b2.id from blob b2 where b2.created_at < now() - interval '1 day'
              and not exists (select 1 from message m where m.blob_id = b2.id)
              and not exists (select 1 from upload u where u.blob_id = b2.id)
              and not exists (select 1 from outbound o where o.blob_id = b2.id)
            limit $limit for update skip locked)
          returning b.id""".query[String].to[List]
  )

  // ---------------------------------------------------------------- メッセージの追加

  /** スレッドの ID（参照している Message-ID のメッセージと同じスレッド。なければ新しく）。 */
  private def threadFor(userId: UUID, msgId: Option[String], refs: List[String]): ConnectionIO[UUID] =
    val ids = (refs ++ msgId.toList).distinct.take(50)
    if ids.isEmpty then FC.pure(UUID.randomUUID())
    else
      sql"select thread_id from message where user_id = $userId and message_id = any($ids) limit 1".query[UUID].option.map(_.getOrElse(UUID.randomUUID()))

  def insertC(userId: UUID, mailboxId: UUID, m: NewMessage): ConnectionIO[MsgMeta] =
    for
      thread <- threadFor(userId, m.messageId, m.references ++ m.inReplyTo.toList)
      alloc <- allocate(mailboxId, 1)
      (uid, modseq) = alloc
      id = UUID.randomUUID()
      row <- (fr"""insert into message as m (id, user_id, mailbox_id, uid, modseq, blob_id, size, internal_date, flags, subject, from_addr, from_name, to_addrs, cc_addrs,
                  sent_at, message_id, in_reply_to, thread_id, has_attachments, preview, search_text, spam_score, spam_report, auth_results)
                values ($id, $userId, $mailboxId, $uid, $modseq, ${m.blobId}, ${m.size}, ${m.internalDate}, ${m.flags.distinct}, ${m.subject.take(1000)},
                  ${m.fromAddr}, ${m.fromName}, ${m.toAddrs.take(4000)}, ${m.ccAddrs.take(4000)}, ${m.sentAt}, ${m.messageId}, ${m.inReplyTo}, $thread,
                  ${m.hasAttachments}, ${m.preview.take(300)}, ${m.searchText}, ${m.spamScore.map(_.toFloat)}, ${m.spamReport}, ${m.authResults})
                returning""" ++ cols).query[MsgMeta].unique
    yield row

  def insert(userId: UUID, mailboxId: UUID, m: NewMessage): IO[MsgMeta] = tx(insertC(userId, mailboxId, m))

  /** 書き出し（mbox）のための、uid の順の一部（after より後を limit 件）: (uid, blob_id, flags, internal_date)。 */
  def exportPage(mailboxId: UUID, after: Long, limit: Int): IO[List[(Long, String, List[String], java.time.Instant)]] = tx(
    sql"select uid, blob_id, flags, internal_date from message where mailbox_id = $mailboxId and uid > $after order by uid limit $limit"
      .query[(Long, String, List[String], java.time.Instant)].to[List]
  )

  /** 容量の上限（既定の値を含む）と使用量。 */
  def quota(userId: UUID, defaultBytes: Long): IO[(Long, Long)] = tx(
    sql"select coalesce(quota_bytes, $defaultBytes), used_bytes from app_user where id = $userId".query[(Long, Long)].option.map(_.getOrElse((defaultBytes, 0L)))
  )

  // ---------------------------------------------------------------- IMAP 向け

  /** メールボックスの UID とフラグ（UID 順）。 */
  def uidFlags(mailboxId: UUID): IO[Vector[UidFlags]] = tx(
    sql"select uid, flags, modseq from message where mailbox_id = $mailboxId order by uid".query[UidFlags].to[Vector]
  )

  /** uid 以下のメッセージの数（IMAP のセッションが消えたものに気づくため）。 */
  def countUpTo(mailboxId: UUID, uid: Long): IO[Int] = tx(
    sql"select count(*)::int from message where mailbox_id = $mailboxId and uid <= $uid".query[Int].unique
  )

  def uidsUpTo(mailboxId: UUID, uid: Long): IO[List[Long]] = tx(
    sql"select uid from message where mailbox_id = $mailboxId and uid <= $uid order by uid".query[Long].to[List]
  )

  def changedSince(mailboxId: UUID, modseq: Long): IO[List[UidFlags]] = tx(
    sql"select uid, flags, modseq from message where mailbox_id = $mailboxId and modseq > $modseq order by uid".query[UidFlags].to[List]
  )

  def byUids(mailboxId: UUID, uids: Seq[Long]): IO[List[MsgMeta]] =
    if uids.isEmpty then IO.pure(Nil)
    else tx((fr"select" ++ cols ++ fr"from message m where m.mailbox_id = $mailboxId and m.uid = any(${uids.toList}) order by m.uid").query[MsgMeta].to[List])

  /** フラグの変更（mode: set | add | remove）。変わった行の UID・フラグ・MODSEQ を返す。 */
  def storeFlags(mailboxId: UUID, uids: Seq[Long], mode: String, flags: List[String], unchangedSince: Option[Long] = None): IO[List[UidFlags]] =
    if uids.isEmpty then IO.pure(Nil)
    else tx {
      val fs = flags.distinct
      val expr = mode match
        case "add"    => fr"array(select distinct unnest(m.flags || $fs::text[]))"
        case "remove" => fr"array(select unnest(m.flags) except select unnest($fs::text[]))"
        case _        => fr"$fs::text[]"
      bumpModseq(mailboxId).flatMap { ms =>
        (fr"update message m set flags = " ++ expr ++ fr", modseq = $ms where m.mailbox_id = $mailboxId and m.uid = any(${uids.toList})" ++
          unchangedSince.fold(Fragment.empty)(u => fr"and m.modseq <= $u") ++
          fr"and not (m.flags @> " ++ expr ++ fr"and m.flags <@ " ++ expr ++ fr") returning m.uid, m.flags, m.modseq").query[UidFlags].to[List]
      }
    }

  /** \Deleted のメッセージを消す（uids を指定すればその中だけ）。消した UID を返す。 */
  def expunge(mailboxId: UUID, uids: Option[Seq[Long]]): IO[List[Long]] = tx(
    bumpModseq(mailboxId) *>
      (fr"delete from message where mailbox_id = $mailboxId and ${skereru.shared.Flag.Deleted} = any(flags)" ++ uids.fold(Fragment.empty)(u => fr"and uid = any(${u.toList})") ++
        fr"returning uid").query[Long].to[List]
  )

  /** 別のメールボックスへ複製する。戻り値は (元の UID, 新しい UID) の組。 */
  def copy(from: UUID, uids: Seq[Long], to: UUID, move: Boolean): IO[List[(Long, Long)]] =
    if uids.isEmpty then IO.pure(Nil)
    else tx(copyC(from, uids, to, move))

  private def copyC(from: UUID, uids: Seq[Long], to: UUID, move: Boolean): ConnectionIO[List[(Long, Long)]] =
    for
      src <- sql"select uid from message where mailbox_id = $from and uid = any(${uids.toList}) order by uid".query[Long].to[List]
      res <- if src.isEmpty then FC.pure(Nil) else
        for
          alloc <- allocate(to, src.size)
          (first, ms) = alloc
          pairs = src.zipWithIndex.map((u, i) => (u, first + i))
          _ <- pairs.traverse_ { (u, nu) =>
            sql"""insert into message (id, user_id, mailbox_id, uid, modseq, blob_id, size, internal_date, flags, subject, from_addr, from_name, to_addrs, cc_addrs,
                    sent_at, message_id, in_reply_to, thread_id, has_attachments, preview, search_text, spam_score, spam_report, auth_results)
                  select ${UUID.randomUUID()}, user_id, $to, $nu, $ms, blob_id, size, internal_date,
                    array(select unnest(flags) except select '\Deleted'), subject, from_addr, from_name, to_addrs, cc_addrs,
                    sent_at, message_id, in_reply_to, thread_id, has_attachments, preview, search_text, spam_score, spam_report, auth_results
                  from message where mailbox_id = $from and uid = $u""".update.run
          }
          _ <- if move then bumpModseq(from) *> sql"delete from message where mailbox_id = $from and uid = any($src)".update.run.void else FC.unit
        yield pairs
    yield res

  // ---------------------------------------------------------------- IMAP の SEARCH

  /** SEARCH の条件を SQL にする（seq は呼び出し側で UID に置き換えておく）。 */
  def search(mailboxId: UUID, key: SearchKey, maxUid: Long, zone: ZoneId): IO[List[Long]] = tx(
    (fr"select m.uid from message m where m.mailbox_id = $mailboxId and" ++ cond(key, maxUid, zone) ++ fr"order by m.uid").query[Long].to[List]
  )

  private def like(s: String): String = s"%${Like.escape(s.toLowerCase)}%"

  private def cond(k: SearchKey, maxUid: Long, zone: ZoneId): Fragment =
    def day(d: java.time.LocalDate) = d.atStartOfDay(zone).toInstant
    k match
      case SearchKey.All => fr"true"
      case SearchKey.Flag(f, true)  => fr"$f = any(m.flags)"
      case SearchKey.Flag(f, false) => fr"not ($f = any(m.flags))"
      case SearchKey.Keyword(f, true)  => fr"$f = any(m.flags)"
      case SearchKey.Keyword(f, false) => fr"not ($f = any(m.flags))"
      case SearchKey.Recent | SearchKey.New => fr"false"
      case SearchKey.Old => fr"true"
      case SearchKey.HeaderContains(name, v) =>
        val p = like(v)
        name.toLowerCase match
          case "from"    => fr"(lower(coalesce(m.from_addr, '')) like $p or lower(coalesce(m.from_name, '')) like $p)"
          case "to"      => fr"lower(m.to_addrs) like $p"
          case "cc"      => fr"lower(m.cc_addrs) like $p"
          case "subject" => fr"lower(m.subject) like $p"
          case "message-id" => fr"lower(coalesce(m.message_id, '')) like $p"
          case "in-reply-to" => fr"lower(coalesce(m.in_reply_to, '')) like $p"
          case _ => if v.isEmpty then fr"true" else fr"false"
      case SearchKey.Body(t) => fr"m.search_text &@ $t"
      case SearchKey.Text(t) =>
        val p = like(t)
        fr"(m.search_text &@ $t or lower(m.subject) like $p or lower(coalesce(m.from_addr, '')) like $p or lower(m.to_addrs) like $p)"
      case SearchKey.Before(d)     => fr"m.internal_date < ${day(d)}"
      case SearchKey.On(d)         => fr"m.internal_date >= ${day(d)} and m.internal_date < ${day(d.plusDays(1))}"
      case SearchKey.Since(d)      => fr"m.internal_date >= ${day(d)}"
      case SearchKey.SentBefore(d) => fr"coalesce(m.sent_at, m.internal_date) < ${day(d)}"
      case SearchKey.SentOn(d)     => fr"coalesce(m.sent_at, m.internal_date) >= ${day(d)} and coalesce(m.sent_at, m.internal_date) < ${day(d.plusDays(1))}"
      case SearchKey.SentSince(d)  => fr"coalesce(m.sent_at, m.internal_date) >= ${day(d)}"
      case SearchKey.Larger(n)     => fr"m.size > $n"
      case SearchKey.Smaller(n)    => fr"m.size < $n"
      case SearchKey.Uid(set)      => uidCond(set, maxUid)
      case SearchKey.Seq(set)      => uidCond(set, maxUid) // 呼び出し側で UID に置き換え済み
      case SearchKey.ModSeqAbove(n) => fr"m.modseq >= $n"
      case SearchKey.Not(x)        => fr"not (" ++ cond(x, maxUid, zone) ++ fr")"
      case SearchKey.Or(a, b)      => fr"((" ++ cond(a, maxUid, zone) ++ fr") or (" ++ cond(b, maxUid, zone) ++ fr"))"
      case SearchKey.And(ks)       => Sql.and(ks.map(cond(_, maxUid, zone)))

  private def uidCond(set: SeqSet, maxUid: Long): Fragment =
    Sql.or(set.bounds(maxUid).map((a, b) => fr"m.uid between $a and $b"))

  // ---------------------------------------------------------------- 画面・API 向け

  def find(userId: UUID, id: UUID): IO[Option[MsgMeta]] = tx(findC(userId, id))
  def findC(userId: UUID, id: UUID): ConnectionIO[Option[MsgMeta]] =
    (fr"select" ++ cols ++ fr"from message m where m.id = $id and m.user_id = $userId").query[MsgMeta].option

  def findMany(userId: UUID, ids: List[UUID]): IO[List[MsgMeta]] =
    if ids.isEmpty then IO.pure(Nil)
    else tx((fr"select" ++ cols ++ fr"from message m where m.user_id = $userId and m.id = any($ids)").query[MsgMeta].to[List])

  /** スレッドのメッセージ（ゴミ箱・迷惑メールは、開いているのがそこでなければ除く）。 */
  def thread(userId: UUID, threadId: UUID, includeTrash: Boolean): IO[List[MsgMeta]] = tx(
    (fr"select" ++ cols ++ fr"from message m join mailbox b on b.id = m.mailbox_id where m.user_id = $userId and m.thread_id = $threadId" ++
      (if includeTrash then Fragment.empty else fr"and coalesce(b.special_use, '') not in ('trash', 'junk')") ++
      fr"order by m.internal_date, m.uid limit 200").query[MsgMeta].to[List]
  )

  /** 一覧（mailboxId か、全体の検索）。conversation のときはスレッドごとに最新の1通（と件数）。 */
  def list(userId: UUID, mailboxId: Option[UUID], q: MailQuery, conversation: Boolean, offset: Int, limit: Int, zone: ZoneId)
      : IO[(List[(MsgMeta, Int)], Int)] = tx {
    def day(d: java.time.LocalDate) = d.atStartOfDay(zone).toInstant
    val inBox: Option[Fragment] = q.in.map { n =>
      val name = SpecialUse.all.find(u => SpecialUse.label(u) == n || u.equalsIgnoreCase(n))
      name.fold(fr"m.mailbox_id in (select id from mailbox where user_id = $userId and lower(name) = ${n.toLowerCase})")(u =>
        fr"m.mailbox_id in (select id from mailbox where user_id = $userId and special_use = $u)")
    }
    val conds: List[Fragment] = List(
      Some(fr"m.user_id = $userId"),
      mailboxId.map(b => fr"m.mailbox_id = $b"),
      // 全体の検索ではゴミ箱と迷惑メールを除く（in: で指定したときは除かない）
      Option.when(mailboxId.isEmpty && q.in.isEmpty)(fr"m.mailbox_id not in (select id from mailbox where user_id = $userId and special_use in ('trash', 'junk'))"),
      inBox
    ).flatten ++
      // 語の数に上限を付ける（LIKE を大量に並べた重い検索をさせない）
      q.words.take(MaxTerms).map(w => fr"(m.search_text &@ $w or lower(m.subject) like ${like(w)} or lower(coalesce(m.from_addr, '')) like ${like(w)} or lower(coalesce(m.from_name, '')) like ${like(w)} or lower(m.to_addrs) like ${like(w)})") ++
      q.from.take(MaxTerms).map(f => fr"(lower(coalesce(m.from_addr, '')) like ${like(f)} or lower(coalesce(m.from_name, '')) like ${like(f)})") ++
      q.to.take(MaxTerms).map(t => fr"(lower(m.to_addrs) like ${like(t)} or lower(m.cc_addrs) like ${like(t)})") ++
      q.subject.take(MaxTerms).map(s => fr"lower(m.subject) like ${like(s)}") ++
      List(
        q.hasAttachment.map(h => fr"m.has_attachments = $h"),
        q.unread.map(u => if u then fr"not ('\Seen' = any(m.flags))" else fr"'\Seen' = any(m.flags)"),
        q.starred.map(s => if s then fr"'\Flagged' = any(m.flags)" else fr"not ('\Flagged' = any(m.flags))"),
        q.before.map(d => fr"m.internal_date < ${day(d)}"),
        q.after.map(d => fr"m.internal_date >= ${day(d)}"),
        q.larger.map(n => fr"m.size > $n"),
        q.smaller.map(n => fr"m.size < $n"),
        // スヌーズ中のものは、is:snoozed のときだけ出す
        Some(if q.snoozed.contains(true) then fr"m.snoozed_until is not null" else fr"m.snoozed_until is null"),
        Some(fr"not ('\Deleted' = any(m.flags))")
      ).flatten
    val where = Sql.whereAnd(conds)
    if conversation then
      val base = fr"select" ++ cols ++ fr", count(*) over (partition by m.thread_id)::int as n, row_number() over (partition by m.thread_id order by m.internal_date desc, m.uid desc) as rn from message m" ++ where
      for
        rows <- (fr"select * from (" ++ base ++ fr") t where t.rn = 1 order by t.internal_date desc limit $limit offset $offset")
                  .query[(MsgMeta, Int, Long)].to[List]
        total <- (fr"select count(distinct m.thread_id)::int from message m" ++ where).query[Int].unique
      yield (rows.map((m, n, _) => (m, n)), total)
    else
      for
        rows <- (fr"select" ++ cols ++ fr", 1 from message m" ++ where ++ fr"order by m.internal_date desc, m.uid desc limit $limit offset $offset")
                  .query[(MsgMeta, Int)].to[List]
        total <- (fr"select count(*)::int from message m" ++ where).query[Int].unique
      yield (rows, total)
  }

  /** メッセージのフラグをまとめて変える（画面・API 向け。メールボックスごとに MODSEQ を進める）。 */
  def setFlags(userId: UUID, ids: List[UUID], add: List[String], remove: List[String]): IO[Int] =
    if ids.isEmpty then IO.pure(0)
    else tx(
      sql"select distinct mailbox_id from message where user_id = $userId and id = any($ids)".query[UUID].to[List].flatMap(_.traverse { b =>
        bumpModseq(b).flatMap { ms =>
          sql"""update message m set flags = array(select distinct unnest(m.flags || $add::text[]) except select unnest($remove::text[])), modseq = $ms
                where m.mailbox_id = $b and m.id = any($ids)""".update.run
        }
      }).map(_.sum)
    )

  /** メッセージを別のメールボックスへ移す（画面・API 向け）。 */
  def moveMessages(userId: UUID, ids: List[UUID], to: UUID): IO[Int] =
    if ids.isEmpty then IO.pure(0)
    else tx(
      sql"select mailbox_id, uid from message where user_id = $userId and id = any($ids) and mailbox_id <> $to".query[(UUID, Long)].to[List].flatMap { rows =>
        rows.groupMap(_._1)(_._2).toList.traverse((from, uids) => copyC(from, uids, to, move = true).map(_.size)).map(_.sum)
      }
    )

  /** メッセージを消す（ゴミ箱を空にする・完全に削除する）。 */
  def deleteMessages(userId: UUID, ids: List[UUID]): IO[Int] =
    if ids.isEmpty then IO.pure(0)
    else tx(
      sql"select distinct mailbox_id from message where user_id = $userId and id = any($ids)".query[UUID].to[List].flatMap(_.traverse_(bumpModseq)) *>
        sql"delete from message where user_id = $userId and id = any($ids)".update.run
    )

  /** メールボックスを空にする（ゴミ箱・迷惑メール）。 */
  def emptyMailbox(boxId: UUID): IO[Int] = tx(bumpModseq(boxId) *> sql"delete from message where mailbox_id = $boxId".update.run)

  /** 古いメッセージを消す（ゴミ箱・迷惑メールで days 日を過ぎたもの）。 */
  def purgeOld(use: String, days: Int): IO[Int] = tx(
    sql"""delete from message m using mailbox b where b.id = m.mailbox_id and b.special_use = $use and m.internal_date < now() - make_interval(days => $days)"""
      .update.run
  )

  // ---------------------------------------------------------------- 添付ファイルの一時保存

  def createUpload(userId: UUID, filename: String, contentType: String, size: Long, blobId: String): IO[UploadDto] = tx(
    sql"""insert into upload (id, user_id, filename, content_type, size, blob_id) values (${UUID.randomUUID()}, $userId, $filename, $contentType, $size, $blobId)
          returning id, filename, content_type, size""".query[UploadDto].unique
  )

  def uploads(userId: UUID, ids: List[UUID]): IO[List[Upload]] =
    if ids.isEmpty then IO.pure(Nil)
    else tx(sql"select id, user_id, filename, content_type, size, blob_id from upload where user_id = $userId and id = any($ids)".query[Upload].to[List])

  /** まだ使っていないアップロードの件数と合計の大きさ。 */
  def pendingUploads(userId: UUID): IO[(Long, Long)] = tx(
    sql"select count(*), coalesce(sum(size), 0)::bigint from upload where user_id = $userId".query[(Long, Long)].unique
  )

  /** 古いアップロードを消す（予約送信で使うものは、送るまで残す）。 */
  def purgeUploads(): IO[Int] = tx(
    sql"""delete from upload where created_at < now() - interval '1 day'
          and id not in (select unnest(upload_ids) from scheduled_send where status = 'queued')""".update.run
  )

  // ---------------------------------------------------------------- スヌーズ

  /** until まで一覧に出さない（None なら戻す）。 */
  def snooze(userId: UUID, ids: List[UUID], until: Option[java.time.Instant]): IO[Int] =
    if ids.isEmpty then IO.pure(0)
    else tx(sql"update message set snoozed_until = $until where user_id = $userId and id = any($ids)".update.run)

  /** 時刻が来たスヌーズを戻す（未読にして、受信の日時を今にする＝一覧の上に出す）。戻したユーザーを返す。 */
  def wakeSnoozed(): IO[List[UUID]] = tx(
    sql"""select id, user_id, mailbox_id from message where snoozed_until <= now() limit 500 for update skip locked"""
      .query[(UUID, UUID, UUID)].to[List].flatMap { rows =>
        val ids = rows.map(_._1)
        if ids.isEmpty then List.empty[UUID].pure[ConnectionIO]
        else
          rows.map(_._3).distinct.traverse_(bumpModseq) *>
            sql"""update message set snoozed_until = null, internal_date = now(), flags = array_remove(flags, '\Seen'),
                  modseq = (select highest_modseq from mailbox b where b.id = message.mailbox_id) where id = any($ids)""".update.run
              .as(rows.map(_._2).distinct)
      }
  )

  // ---------------------------------------------------------------- 統計

  def stats: IO[StatsDto] = tx(
    for
      users <- sql"select count(*)::int from app_user".query[Int].unique
      doms  <- sql"select count(*)::int from domain".query[Int].unique
      nb <- sql"select count(*), coalesce(sum(size), 0)::int8 from message".query[(Long, Long)].unique
      q <- sql"select count(*) filter (where status = 'queued')::int, count(*) filter (where status = 'failed')::int from outbound".query[(Int, Int)].unique
      l <- sql"""select count(*) filter (where direction = 'in' and result = 'delivered')::int, count(*) filter (where direction = 'in' and result = 'junk')::int,
                   count(*) filter (where direction = 'in' and result = 'rejected')::int, count(*) filter (where direction <> 'in' and result = 'sent')::int
                 from mail_log where at > now() - interval '1 day'""".query[(Int, Int, Int, Int)].unique
    yield StatsDto(users, doms, nb._1, nb._2, q._1, q._2, l._1, l._2, l._3, l._4)
  )

object MailStore:
  /** 1人のメールボックスの数の上限。 */
  val MaxMailboxes = 1000
