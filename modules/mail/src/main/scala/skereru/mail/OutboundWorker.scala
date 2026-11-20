package skereru.mail

import sbirds.db.*
import sbirds.observe.Log

import skereru.core.ErrorCode

import cats.effect.*
import cats.syntax.all.*
import skereru.db.*

import scala.concurrent.duration.*

/** 送信待ちの処理（全ノードで動き、SKIP LOCKED で取り合わない）。送れなかったものは差出人に知らせる。 */
final class OutboundWorker(queue: QueueStore,
                           blobs: skereru.storage.Blobs,
                           client: SmtpClient,
                           delivery: Delivery,
                           cfg: MailConfig,
                           log: String => Unit) {

  def loop: IO[Nothing] =
    (IO.sleep(5.seconds) *>
      (runOnce.handleErrorWith(e => Log.report(ErrorCode.OutboundWorkerFailed, e.toString, e)) *>
        IO.sleep(10.seconds)).foreverM)

  def expireLoop: IO[Nothing] =
    Periodic.run(1.minute, 10.minutes, ErrorCode.OutboundExpireFailed)(
      queue.expire(cfg.queueLifetimeHours).flatMap(_.groupBy(o => (o.sender, o.blobId)).toList.traverse_ {
        case ((sender, blobId), os) =>
          blobs.get(blobId).flatMap(_.traverse_(raw =>
            delivery.bounceToSender(sender, raw, os.map(o => o.recipient -> o.lastError.getOrElse("配送できませんでした")))
          ))
      })
    )

  def runOnce: IO[Int] =
    queue.processDue(50) { batch =>
      val o = batch.head
      blobs.get(o.blobId).flatMap {
        case None => IO.pure(batch.map(_.id -> OutboxResult.Skip("本体がありません")).toMap)
        case Some(raw) =>
          sbirds.observe.Telemetry.span(
            "smtp.outbound",
            "mail.domain" -> o.domain,
            "mail.rcpts" -> batch.size.toString
          )(IO.blocking(client.deliver(o.domain, o.sender, batch.map(_.recipient), raw))).flatMap { res =>
            val perm =
              batch.flatMap(x => res.get(x.recipient).collect { case RcptResult.Perm(m) => x.recipient -> m })
            val sent = batch.count(x => res.get(x.recipient).contains(RcptResult.Ok))
            val temp = batch.size - sent - perm.size
            // 外への送信の結果（宛先ごと）
            List("sent" -> sent, "bounced" -> perm.size, "tempfail" -> temp).traverse_ {
              (r, n) =>
                IO.whenA(n > 0)(sbirds.observe.Metrics.add("skereru.mail.sent", n.toLong, "result" -> r))
            } *>
              queue.log(
                "out",
                if perm.nonEmpty then "bounced" else if sent > 0 then "sent" else "tempfail",
                None,
                o.sender,
                batch.map(_.recipient),
                raw.length,
                None,
                res.values.collect { case RcptResult.Temp(m) => m; case RcptResult.Perm(m) => m }.mkString(
                  " | "
                ).take(500)
              ) *>
              IO.whenA(perm.nonEmpty)(delivery.bounceToSender(o.sender, raw, perm)).as(batch.map { x =>
                x.id ->
                  (res.getOrElse(x.recipient, RcptResult.Temp("未処理")) match {
                    case RcptResult.Ok => OutboxResult.Sent
                    case RcptResult.Perm(m) => OutboxResult.Skip(m.take(1000))
                    case RcptResult.Temp(m) => OutboxResult.Retry(m.take(1000))
                  })
              }.toMap)
          }
      }
    }
}
