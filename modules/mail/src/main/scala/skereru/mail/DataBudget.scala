package skereru.mail

/**
 * DATA の本文を溜めるのに、全接続で使ってよいメモリの量（ヒープの 1/4）。
 * 1接続ごとの上限（maxMessageBytes）だけだと、終わりの "." を送らない接続を並べてメモリを使い切らせられるため。
 */
object DataBudget {
  private val limit = Runtime.getRuntime.maxMemory() / 4
  private val used = new java.util.concurrent.atomic.AtomicLong(0)

  /** n バイトを借りる。足りなければ false。 */
  def acquire(
    n: Long
  ): Boolean = {
    val after = used.addAndGet(n)
    if after > limit then { used.addAndGet(-n); false }
    else true
  }
  def release(
    n: Long
  ): Unit = used.addAndGet(-n)
}
