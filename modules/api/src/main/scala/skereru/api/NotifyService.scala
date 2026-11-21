package skereru.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.client.Client
import sbirds.auth.Crypto
import sbirds.db.{OutboxResult, Periodic}
import sbirds.links.{StuiClient, StuiMessage, Webhook}
import sbirds.net.NetGuard
import sbirds.shared.AppError
import skereru.auth.SecretBox
import skereru.core.{Address, ErrorCode, Mime, MimePart}
import skereru.db.*
import skereru.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

/**
 * 受信の通知（stui・受信 Webhook・署名付きの Webhook）.
 * 受信トレイに入った新着（allInbox の通知先）と、振り分けの規則の「通知先に知らせる」で選んだ通知先に、差出人・件名・リンクを送る.
 * 本文は送らない（チャンネルの全員に見えるため）. 送信は送信待ちの表（notify_delivery）に積み、裏のワーカーが送る
 * （複数ノードは SKIP LOCKED で取り合う. 失敗したら間を空けて送り直す）.
 *  - stui: 本人が OAuth で連携したトークンで、stui のボット（skereru）から本人への DM か、本人が参加しているチャンネルへ投稿する.
 *  - chat_hook: stui・Slack・Mattermost の受信 Webhook に {"text": …, "username": …} を送る（URL が鍵）.
 *  - webhook: イベント mail.received の JSON を、HMAC-SHA256 の署名（X-Skereru-Signature）付きで送る.
 * 利用者が決めた URL へは hookHttp（NetGuard.client）で送り、内部のアドレスは policy（NOTIFY_ALLOW_INTERNAL）で許したものだけにする.
 */
final class NotifyService(store: NotifyStore,
                          stui: Option[StuiClient],
                          hookHttp: Client[IO],
                          policy: NetGuard.Policy,
                          box: SecretBox,
                          publicUrl: String) {

  /** 試しの通知に使うイベントの名前. */
  val TestEvent = "test"
  val MailEvent = "mail.received"

  def list(userId: UUID): IO[List[NotifyTargetDto]] = store.targets(userId).map(_.map(dto))

  def create(userId: UUID, r: NotifyTargetReq): Res[NotifyTargetDto] =
    store.count(userId).flatMap { n =>
      if n >= store.MaxTargets then IO.pure(Left(AppError(ErrorCode.NotifyTargetTooMany)))
      else
        validate(r).flatMap(_.traverse { v =>
          val row = NotifyTargetRow(
            UUID.randomUUID(),
            userId,
            v.name,
            v.kind,
            v.channelId,
            v.channelName,
            v.url,
            Option.when(v.kind == NotifyKind.Webhook)(box.encrypt(Crypto.randomToken(24))),
            v.allInbox,
            v.enabled,
            None,
            Instant.now()
          )
          store.insert(row).as(dto(row))
        })
    }

  /** 種類は変えられない（変えるときは作り直す）. */
  def update(userId: UUID, id: UUID, r: NotifyTargetReq): Res[NotifyTargetDto] =
    store.target(userId, id).flatMap {
      case None => IO.pure(Left(AppError(ErrorCode.NotifyTargetNotFound)))
      case Some(t) =>
        validate(r.copy(kind = t.kind)).flatMap(_.flatTraverse(v =>
          store.update(userId, id, v) *>
            store.target(userId, id).map(_.map(dto).toRight(AppError(ErrorCode.NotifyTargetNotFound)))
        ))
    }

  def delete(userId: UUID, id: UUID): Res[Unit] =
    store.delete(userId, id).map(ok => Either.cond(ok, (), AppError(ErrorCode.NotifyTargetNotFound)))

  /** 試しに送る（送信待ちを通さず、その場で送って結果を返す）. */
  def test(userId: UUID, id: UUID, userName: String): Res[Unit] =
    store.target(userId, id).flatMap {
      case None => IO.pure(Left(AppError(ErrorCode.NotifyTargetNotFound)))
      case Some(t) =>
        val payload = Json.obj(
          "event" -> TestEvent.asJson,
          "message" -> s"$userName さんの skereru からの試しの通知です".asJson,
          "url" -> publicUrl.stripSuffix("/").asJson
        )
        send(t, UUID.randomUUID(), payload).map {
          case (OutboxResult.Sent, _) => Right(())
          case (OutboxResult.Skip(m), _) => Left(AppError(ErrorCode.NotifyTestFailed, Some(m)))
          case (OutboxResult.Retry(m), _) => Left(AppError(ErrorCode.NotifyTestFailed, Some(m)))
        }
    }

  /**
   * メールが届いた（配送の後に裏で呼ぶ）. inbox は受信トレイに入った新着か（迷惑メール・既読にする規則のものは false）.
   * ruleTargets は振り分けの規則で選んだ通知先の ID.
   */
  def mailArrived(userId: UUID,
                  messageId: UUID,
                  root: MimePart,
                  mailbox: String,
                  inbox: Boolean,
                  ruleTargets: List[String]): IO[Unit] = {
    val ids = ruleTargets.flatMap(x => scala.util.Try(UUID.fromString(x)).toOption)
    IO.whenA(inbox || ids.nonEmpty)(store.targetsFor(userId, inbox, ids).flatMap(ts =>
      IO.whenA(ts.nonEmpty) {
        val p = NotifyService.mailPayload(messageId, root, mailbox, Instant.now(), publicUrl)
        ts.traverse_(t => store.enqueue(t.id, p))
      }
    ))
  }

  /** 送信待ちの1件を送る. */
  def deliver(d: NotifyDeliveryRow): IO[(OutboxResult, Option[Int])] =
    sbirds.observe.Telemetry.span("notify.deliver", "notify.kind" -> d.target.kind)(send(d.target, d.id, d.payload))

  private def send(t: NotifyTargetRow, deliveryId: UUID, payload: Json): IO[(OutboxResult, Option[Int])] =
    t.kind match {
      case NotifyKind.Stui =>
        stui match {
          case None => IO.pure((OutboxResult.Skip("このサーバーでは stui と連携できません"), None))
          case Some(c) => c.notify(t.userId, NotifyService.stuiMessage(payload), t.channelId)
        }
      case NotifyKind.ChatHook =>
        t.url match {
          case None => IO.pure((OutboxResult.Skip("URL がありません"), None))
          case Some(u) =>
            NetGuard.check(u, policy).flatMap {
              case Left(r) => IO.pure((OutboxResult.Skip(r.message), None))
              case Right(_) =>
                // 見出しのエスケープは lib（StuiMessage.markdown）が行う
                StuiClient.postHook(hookHttp, u, NotifyService.stuiMessage(payload), "skereru")
            }
        }
      case NotifyKind.Webhook =>
        (t.url, t.secretEnc.flatMap(box.decrypt)) match {
          case (Some(u), Some(sec)) =>
            val event = payload.hcursor.get[String]("event").getOrElse(MailEvent)
            Webhook.post(
              hookHttp,
              policy,
              "Skereru",
              Webhook.Send(u, event, deliveryId.toString, payload.noSpaces, sec)
            )
          case _ => IO.pure((OutboxResult.Skip("URL か署名の鍵がありません"), None))
        }
      case k => IO.pure((OutboxResult.Skip(s"知らない種類です: $k"), None))
    }

  /** 入力の確認（名前・種類・送り先）. URL は送れる宛先か（内部のアドレスでないか）も確かめる. */
  private def validate(r: NotifyTargetReq): Res[NotifyTargetReq] = {
    val name = r.name.trim.take(100)
    val url = r.url.map(_.trim).filter(_.nonEmpty)
    val bad = (m: String) => IO.pure(Left(AppError(ErrorCode.NotifyTargetInvalid, Some(m))))
    if name.isEmpty then bad("名前を入力してください")
    else
      r.kind match {
        case NotifyKind.Stui =>
          if stui.isEmpty then IO.pure(Left(AppError(ErrorCode.StuiNotConfigured)))
          else
            IO.pure(Right(r.copy(
              name = name,
              channelId = r.channelId.map(_.trim).filter(_.nonEmpty),
              channelName = r.channelName.map(_.trim.take(100)).filter(_.nonEmpty),
              url = None
            )))
        case NotifyKind.ChatHook | NotifyKind.Webhook =>
          url match {
            case None => bad("送り先の URL を入力してください")
            case Some(u) if u.length > 2000 => bad("URL が長すぎます")
            case Some(u) if !(u.startsWith("https://") || u.startsWith("http://")) =>
              bad("URL は https:// で始めてください")
            case Some(u) =>
              NetGuard.check(u, policy).map {
                case Left(e) => Left(AppError(ErrorCode.NotifyUrlRejected, Some(e.message)))
                case Right(_) => Right(r.copy(name = name, url = Some(u), channelId = None, channelName = None))
              }
          }
        case _ => bad("通知先の種類が正しくありません")
      }
  }

  private def dto(t: NotifyTargetRow): NotifyTargetDto =
    NotifyTargetDto(
      t.id,
      t.name,
      t.kind,
      t.channelId,
      t.channelName,
      t.url,
      t.secretEnc.flatMap(box.decrypt),
      t.allInbox,
      t.enabled,
      t.lastError,
      t.createdAt
    )

  /** 送信待ちを送るワーカー（どのノードで動かしてもよい）. */
  def loop: IO[Nothing] =
    Periodic.run(15.seconds, 5.seconds, ErrorCode.NotifyWorkerFailed)(store.processDeliveries(50)(deliver))

  def purgeLoop: IO[Nothing] = Periodic.run(1.hour, 24.hours, ErrorCode.NotifyPurgeFailed)(store.purge(14))
}

object NotifyService {

  /**
   * イベント mail.received の本文（Webhook にはこのまま送る）.
   * {"event", "id", "receivedAt", "mailbox", "from": {"name", "address"}, "subject", "url"}
   */
  def mailPayload(id: UUID, root: MimePart, mailbox: String, at: Instant, publicUrl: String): Json = {
    val from = root.header("From").flatMap(Address.parseOne)
    val subject = root.header("Subject").map(Mime.decodeHeader).map(_.trim).filter(_.nonEmpty)
    Json.obj(
      "event" -> "mail.received".asJson,
      "id" -> id.asJson,
      "receivedAt" -> at.toString.asJson,
      "mailbox" -> mailbox.asJson,
      "from" -> Json.obj(
        "name" -> from.flatMap(_.nameDecoded).asJson,
        "address" -> from.map(_.address).asJson
      ),
      "subject" -> subject.asJson,
      "url" -> s"${publicUrl.stripSuffix("/")}/m/$id".asJson
    )
  }

  /**
   * stui に投稿する文（見出しは件名、本文は差出人と、受信トレイ以外ならフォルダ）. 本文の Markdown の記号は無害にする.
   * 見出しはエスケープしない（POST /api/apps/notify では stui が、受信 Webhook では StuiMessage.markdown が見出しをエスケープする）.
   */
  def stuiMessage(p: Json): StuiMessage = {
    val h = p.hcursor
    val url = h.get[String]("url").getOrElse("")
    if h.get[String]("event").contains("test") then
      StuiMessage("skereru からの試しの通知", StuiMessage.plain(h.get[String]("message").getOrElse("")), url)
    else {
      val subject = h.get[Option[String]]("subject").toOption.flatten.getOrElse("(件名なし)")
      val name = h.downField("from").get[Option[String]]("name").toOption.flatten
      val addr = h.downField("from").get[Option[String]]("address").toOption.flatten
      val from = (name, addr) match {
        case (Some(n), Some(a)) => s"$n <$a>"
        case (n, a) => n.orElse(a).getOrElse("（不明）")
      }
      val box = h.get[String]("mailbox").toOption.filterNot(_.equalsIgnoreCase("INBOX"))
      val text = (s"差出人: ${StuiMessage.plain(from)}" :: box.map(b => s"フォルダ: ${StuiMessage.plain(b)}").toList).mkString(
        "\n"
      )
      StuiMessage(s"📩 ${subject.replaceAll("[\\r\\n]+", " ")}".take(300), text, url)
    }
  }
}
