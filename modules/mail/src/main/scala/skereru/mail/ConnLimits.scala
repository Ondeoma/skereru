package skereru.mail

import java.net.InetAddress
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentHashMap

/** 接続数の制限（全体・IP ごと・広い単位ごと）。IP ごとの数は、PROXY プロトコルで知った本当の接続元で数える。 */
final class ConnLimits(maxTotal: Int, maxPerIp: Int) {
  private val total = new AtomicInteger(0)
  private val perIp = new ConcurrentHashMap[String, AtomicInteger]()
  private val maxPerNet = maxPerIp * 4

  def acquireTotal(): Boolean =
    if total.incrementAndGet() > maxTotal then { total.decrementAndGet(); false }
    else true

  def releaseTotal(): Unit = total.decrementAndGet()

  private def acquire(k: String, max: Int): Boolean = {
    val c = perIp.computeIfAbsent(k, _ => new AtomicInteger(0))
    if c.incrementAndGet() > max then { c.decrementAndGet(); false }
    else true
  }

  private def release(k: String): Unit = {
    val c = perIp.get(k)
    if c != null && c.decrementAndGet() <= 0 then perIp.remove(k, c)
  }

  def acquireIp(ip: InetAddress): Boolean =
    if !acquire(IpKey.of(ip), maxPerIp) then false
    else if !acquire("net:" + IpKey.wide(ip), maxPerNet) then { release(IpKey.of(ip)); false }
    else true

  def releaseIp(ip: InetAddress): Unit = {
    release(IpKey.of(ip)); release("net:" + IpKey.wide(ip))
  }

  def current: Int = total.get()
}
