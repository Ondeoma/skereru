package skereru.mail

import cats.effect.*
import cats.effect.std.Dispatcher
import sbirds.observe.{Metrics, OpenSpan, Telemetry}

/** 仮想スレッドのブロッキングな処理から、IO（DB など）を呼ぶ橋渡し。scope があれば、動かす IO はそのスパンの子になる。 */
final class Bridge(
  d: Dispatcher[IO],
  scope: Option[OpenSpan] = None) {
  def run[A](
    io: IO[A]
  ): A = d.unsafeRunSync(scope.fold(io)(_.within(io)))

  /** body（セッション1つなど）をスパンで囲む。body に渡す Bridge で動かした IO は、このスパンの子になる。 */
  def span[A](
    name: String,
    kv: (String, String)*
  )(
    body: Bridge => A
  ): A = {
    val (s, close) = run(Telemetry.open(name, kv*).allocated)
    try body(new Bridge(d, Some(s)))
    catch {
      case e: (java.io.IOException | LineTooLong) => throw e // 切断・タイムアウトはふつうの終わり
      case e: Throwable => { run(s.fail(e)); throw e }
    } finally run(close)
  }

  /** body の間だけ、メトリクス name（接続数など）を1つ増やす。 */
  def tracking[A](
    name: String,
    kv: (String, String)*
  )(
    body: => A
  ): A = {
    run(Metrics.upDown(name, 1, kv*))
    try body
    finally run(Metrics.upDown(name, -1, kv*))
  }
}
