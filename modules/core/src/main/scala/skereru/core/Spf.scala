package skereru.core

import java.net.{Inet4Address, Inet6Address, InetAddress}

/** SPF（RFC 7208）の評価。 */
final class Spf(
  dns: Dns) {

  private final class Ctx(
    val ip: InetAddress,
    val sender: String,
    val helo: String) {
    var lookups = 0
    var voids = 0

    /** 評価全体にかけてよい時間（mx の各ホストの問い合わせは回数に数えないので、遅い DNS で長く止められないように）。 */
    val deadline = System.currentTimeMillis() + 20_000
  }

  private val Modifier = "^[A-Za-z][A-Za-z0-9_.-]*=".r

  private final class Abort(
    val r: SpfResult) extends Exception(null, null, false, false)

  /** sender は MAIL FROM（空なら postmaster@helo）。 */
  def check(
    ip: InetAddress,
    sender: String,
    helo: String
  ): SpfResult = {
    val s = if sender.isEmpty then s"postmaster@$helo" else sender
    val ctx = Ctx(ip, s, helo)
    try checkHost(ctx, Address.domainOf(s), 0)
    catch case a: Abort => a.r
  }

  private def countLookup(
    ctx: Ctx
  ): Unit = {
    if System.currentTimeMillis() > ctx.deadline then throw Abort(SpfResult.TempError)
    ctx.lookups += 1
    if ctx.lookups > 10 then throw Abort(SpfResult.PermError)
  }

  private def void(
    ctx: Ctx,
    empty: Boolean
  ): Unit =
    if empty then {
      ctx.voids += 1
      if ctx.voids > 2 then throw Abort(SpfResult.PermError)
    }

  private def record(
    domain: String
  ): Either[SpfResult, Option[String]] =
    dns.txt(domain) match {
      case Left(_) => Left(SpfResult.TempError)
      case Right(txts) =>
        txts.filter(t => t.equalsIgnoreCase("v=spf1") || t.toLowerCase.startsWith("v=spf1 ")) match {
          case Nil => Right(None)
          case r :: Nil => Right(Some(r))
          case _ => Left(SpfResult.PermError)
        }
    }

  private def checkHost(
    ctx: Ctx,
    domain: String,
    depth: Int
  ): SpfResult = {
    if depth > 10 || domain.isEmpty || domain.length > 253 || domain.split('.').exists(_.isEmpty) then
      return SpfResult.None
    record(domain) match {
      case Left(r) => r
      case Right(None) => SpfResult.None
      case Right(Some(rec)) =>
        val terms = rec.split("\\s+").toList.drop(1).filter(_.nonEmpty)
        val redirect = terms.collectFirst { case t if t.toLowerCase.startsWith("redirect=") => t.drop(9) }
        val directives = terms.filterNot(t => Modifier.findPrefixOf(t).isDefined)
        val it = directives.iterator
        while it.hasNext do {
          val d = it.next()
          val (q, mech) = d.head match {
            case '+' => (SpfResult.Pass, d.tail)
            case '-' => (SpfResult.Fail, d.tail)
            case '~' => (SpfResult.SoftFail, d.tail)
            case '?' => (SpfResult.Neutral, d.tail)
            case _ => (SpfResult.Pass, d)
          }
          if matches(ctx, domain, mech, depth) then return q
        }
        redirect match {
          case Some(r) =>
            countLookup(ctx)
            checkHost(ctx, expand(r, ctx, domain), depth + 1) match {
              case SpfResult.None => SpfResult.PermError
              case other => other
            }
          case None => SpfResult.Neutral
        }
    }
  }

  private def splitCidr(
    s: String
  ): (String, Option[Int], Option[Int]) = {
    // domain/24//64
    val i6 = s.indexOf("//")
    val (rest, c6) = if i6 >= 0 then (s.substring(0, i6), s.substring(i6 + 2).toIntOption) else (s, None)
    val i4 = rest.lastIndexOf('/')
    if i4 >= 0 then (rest.substring(0, i4), rest.substring(i4 + 1).toIntOption, c6) else (rest, None, c6)
  }

  private def matches(
    ctx: Ctx,
    domain: String,
    mech0: String,
    depth: Int
  ): Boolean = {
    val lower = mech0.toLowerCase
    val (name, arg) = lower.indexOf(':') match {
      case -1 => lower.indexOf('/') match {
          case -1 => (lower, "")
          case k => (lower.substring(0, k), mech0.substring(k))
        }
      case k => (lower.substring(0, k), mech0.substring(k + 1))
    }
    name match {
      case "all" => true
      case "include" =>
        countLookup(ctx)
        checkHost(ctx, expand(arg, ctx, domain), depth + 1) match {
          case SpfResult.Pass => true
          case SpfResult.TempError => throw Abort(SpfResult.TempError)
          case SpfResult.PermError | SpfResult.None => throw Abort(SpfResult.PermError)
          case _ => false
        }
      case "a" =>
        countLookup(ctx)
        val (d, c4, c6) = splitCidr(if arg.startsWith("/") then domain + arg else if arg.isEmpty then domain else arg)
        ipsOf(ctx, expand(d, ctx, domain)).exists(inCidr(ctx.ip, _, c4, c6))
      case "mx" =>
        countLookup(ctx)
        val (d, c4, c6) = splitCidr(if arg.startsWith("/") then domain + arg else if arg.isEmpty then domain else arg)
        dns.mx(expand(d, ctx, domain)) match {
          case Left(_) => throw Abort(SpfResult.TempError)
          case Right(mxs) =>
            void(ctx, mxs.isEmpty)
            if mxs.size > 10 then throw Abort(SpfResult.PermError)
            mxs.exists(
              (
                _,
                h
              ) => ipsOf(ctx, h).exists(inCidr(ctx.ip, _, c4, c6))
            )
        }
      case "ip4" | "ip6" =>
        val (a, c4, c6) = splitCidr(arg)
        val bits = if name == "ip4" then c4 else c4.orElse(c6)
        scala.util.Try(InetAddress.getByName(a)).toOption.exists(net => inCidr(ctx.ip, net, bits, bits))
      case "exists" =>
        countLookup(ctx)
        dns.addresses(expand(arg, ctx, domain)) match {
          case Left(_) => throw Abort(SpfResult.TempError)
          case Right(as) => void(ctx, as.isEmpty); as.exists(_.isInstanceOf[Inet4Address])
        }
      case "ptr" =>
        countLookup(ctx)
        val target = if arg.isEmpty then domain else expand(arg, ctx, domain)
        dns.ptr(ctx.ip).getOrElse(Nil).take(10).exists { n =>
          val host = n.stripSuffix(".").toLowerCase
          (host == target || host.endsWith("." + target)) && ipsOf(ctx, host).contains(ctx.ip)
        }
      case _ => throw Abort(SpfResult.PermError)
    }
  }

  private def ipsOf(
    ctx: Ctx,
    host: String
  ): List[InetAddress] = {
    if System.currentTimeMillis() > ctx.deadline then throw Abort(SpfResult.TempError)
    dns.addresses(host) match {
      case Left(_) => throw Abort(SpfResult.TempError)
      case Right(as) => void(ctx, as.isEmpty); as
    }
  }

  private def inCidr(
    ip: InetAddress,
    net: InetAddress,
    c4: Option[Int],
    c6: Option[Int]
  ): Boolean =
    (ip, net) match {
      case (_: Inet4Address, _: Inet4Address) => Cidr.contains(net, c4.getOrElse(32), ip)
      case (_: Inet6Address, _: Inet6Address) => Cidr.contains(net, c6.getOrElse(128), ip)
      case _ => false
    }

  /** マクロ（%{s} %{l} %{o} %{d} %{i} %{h} %{v}）の展開。 */
  private def expand(
    spec: String,
    ctx: Ctx,
    domain: String
  ): String = {
    if !spec.contains('%') then return spec.toLowerCase
    val out = new StringBuilder
    var i = 0
    while i < spec.length do
      spec(i) match {
        case '%' if i + 1 < spec.length && spec(i + 1) == '%' => out.append('%'); i += 2
        case '%' if i + 1 < spec.length && spec(i + 1) == '_' => out.append(' '); i += 2
        case '%' if i + 1 < spec.length && spec(i + 1) == '-' => out.append("%20"); i += 2
        case '%' if i + 1 < spec.length && spec(i + 1) == '{' =>
          val j = spec.indexOf('}', i)
          if j < 0 then throw Abort(SpfResult.PermError)
          val body = spec.substring(i + 2, j)
          val letter = body.headOption.map(_.toLower).getOrElse(throw Abort(SpfResult.PermError))
          val rest = body.drop(1)
          val digits = rest.takeWhile(_.isDigit).toIntOption
          val reverse = rest.dropWhile(_.isDigit).headOption.exists(_.toLower == 'r')
          val delims = rest.dropWhile(_.isDigit).dropWhile(c => c.toLower == 'r')
          val value = letter match {
            case 's' => ctx.sender
            case 'l' => Address.localOf(ctx.sender)
            case 'o' => Address.domainOf(ctx.sender)
            case 'd' => domain
            case 'h' => ctx.helo
            case 'v' => if ctx.ip.isInstanceOf[Inet4Address] then "in-addr" else "ip6"
            case 'p' => "unknown"
            case 'i' => ctx.ip match {
                case a: Inet4Address => a.getHostAddress
                case a =>
                  a.getAddress.flatMap(b => List((b >> 4) & 0xf, b & 0xf)).map(Integer.toHexString).mkString(".")
              }
            case _ => throw Abort(SpfResult.PermError)
          }
          val ds = if delims.isEmpty then "." else delims
          val parts0 = value.split(ds.map(c => "\\" + c).mkString("[", "", "]")).toList
          val parts1 = if reverse then parts0.reverse else parts0
          val parts2 = digits.fold(parts1)(n => parts1.takeRight(math.max(1, n)))
          out.append(parts2.mkString("."))
          i = j + 1
        case c => out.append(c); i += 1
      }
    out.toString.toLowerCase
  }
}
