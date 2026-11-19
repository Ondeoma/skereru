package skereru.api

import java.util.UUID

import cats.effect.*
import fs2.Stream
import skereru.db.*

import scala.concurrent.duration.*

/** 変更の通知。同じノードの配送はすぐに、ほかのノードの変更は数秒ごとの確認で知らせる。 */
final class ChangeHub(
  mail: MailStore,
  topic: fs2.concurrent.Topic[IO, java.util.UUID]) {
  def publish(
    userId: java.util.UUID
  ): IO[Unit] = topic.publish1(userId).void

  /** 1人が同時に開ける通知の数（つなぐたびに数秒ごとの DB の問い合わせが増えるので、上限を付ける）。 */
  private val MaxPerUser = 20
  private val open =
    new java.util.concurrent.ConcurrentHashMap[java.util.UUID, java.util.concurrent.atomic.AtomicInteger]()

  def events(
    userId: java.util.UUID
  ): IO[Option[Stream[IO, String]]] = IO {
    val c = open.computeIfAbsent(userId, _ => new java.util.concurrent.atomic.AtomicInteger(0))
    if c.incrementAndGet() > MaxPerUser then { c.decrementAndGet(); None }
    else Some(stream(userId).onFinalize(IO { if c.decrementAndGet() <= 0 then open.remove(userId, c) }))
  }

  private def stream(
    userId: java.util.UUID
  ): Stream[IO, String] = {
    val local = topic.subscribe(64).filter(_ == userId).as(())
    val ticks = Stream.awakeEvery[IO](4.seconds).as(())
    val tokens = (Stream.emit(()) ++ local.merge(ticks)).evalMap(_ => mail.changeToken(userId)).changes.drop(1).map(t =>
      s"event: change\ndata: $t\n\n"
    )
    // 切断に気付くのは書き込みに失敗したときなので、短めの間隔で送る（閉じた接続の数をすぐに戻す）
    val keepAlive = Stream.awakeEvery[IO](10.seconds).as(": ping\n\n")
    Stream.emit("retry: 5000\n\n") ++ tokens.mergeHaltBoth(keepAlive)
  }
}
