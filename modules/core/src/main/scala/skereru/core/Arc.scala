package skereru.core

import java.net.InetAddress
import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, PrivateKey, Signature}
import java.util.Base64

/** ARC の鎖の検証結果（result は none・pass・fail。instance は最も新しいセットの番号、ARC がなければ 0）。 */
final case class ArcResult(result: String /* none | pass | fail */, instance: Int, reason: String)

/** ARC（Authenticated Received Chain、RFC 8617）の検証。DKIM の正規化・鍵の読み込みを使い回す。メッセージの改行は CRLF であること。 */
final class Arc(dns: Dns):

  private val dkim = new Dkim(dns)

  /** 届いたメッセージの ARC の鎖を検証する（cv の値に使う）。ARC のヘッダーがなければ none。 */
  def verify(msg: Array[Byte], now: java.time.Instant): ArcResult =
    try verifyChain(msg)
    catch case e: Exception => ArcResult("fail", 0, s"検証中のエラー: ${e.getMessage}")

  private def verifyChain(msg: Array[Byte]): ArcResult =
    val (headers, bodyStart) = RawHeaders.split(msg)
    val arcHs = headers.filter(h => Arc.isArc(h.name))
    if arcHs.isEmpty then return ArcResult("none", 0, "")
    Arc.collect(dkim, arcHs) match
      case Left(why) => ArcResult("fail", Arc.highest(dkim, arcHs), why)
      case Right(sets) =>
        val n = sets.length
        def fail(why: String) = ArcResult("fail", n, why)
        // 構造の確認（1番目の cv は none、それより後は pass。fail があれば鎖はもう壊れている）
        val cvs = sets.map(s => dkim.tags(Arc.value(s.as)).getOrElse("cv", "").toLowerCase)
        if cvs.head != "none" then return fail("i=1 の ARC-Seal の cv が none ではありません")
        cvs.zipWithIndex.drop(1).find(_._1 != "pass") match
          case Some((cv, k)) => return fail(s"i=${k + 1} の ARC-Seal の cv が pass ではありません（$cv）")
          case None          => ()
        // 最も新しい ARC-Message-Signature の検証
        verifyAms(msg, headers, bodyStart, sets.last) match
          case Some(why) => fail(why)
          case None =>
            // ARC-Seal を新しいものから順に検証する
            (n to 1 by -1).iterator.map(k => verifySeal(sets, k)).collectFirst { case Some(w) => w } match
              case Some(why) => fail(why)
              case None      => ArcResult("pass", n, "")

  /** ARC-Message-Signature の検証（DKIM の検証とほぼ同じ。v= はなく、ARC-Seal・ARC-Authentication-Results を署名してはいけない）。 */
  private def verifyAms(msg: Array[Byte], headers: List[RawHeader], bodyStart: Int, set: Arc.ArcSet): Option[String] =
    val t = dkim.tags(Arc.value(set.ams))
    val d = t.getOrElse("d", "").toLowerCase
    val s = t.getOrElse("s", "")
    val algo = t.getOrElse("a", "").toLowerCase
    if d.isEmpty || s.isEmpty || !t.contains("b") || !t.contains("bh") || !t.contains("h") then
      return Some(s"i=${set.i} の ARC-Message-Signature に必須のタグがありません")
    val names = t("h").split(':').map(_.trim.toLowerCase).filter(_.nonEmpty).toList
    if !names.contains("from") then return Some("ARC-Message-Signature で From が署名されていません")
    if names.exists(n => n == "arc-seal" || n == "arc-authentication-results") then
      return Some("ARC-Message-Signature が ARC-Seal・ARC-Authentication-Results を署名しています")
    if !Arc.Algorithms.contains(algo) then return Some(s"ARC-Message-Signature の未対応のアルゴリズム $algo")
    val (hc, bc) = t.getOrElse("c", "simple/simple").toLowerCase.split('/') match
      case Array(a, b) => (a, b)
      case Array(a)    => (a, "simple")
      case _           => ("simple", "simple")
    // 本文のハッシュ（l= には対応しない。全体を使う）
    val body = dkim.canonBody(java.util.Arrays.copyOfRange(msg, bodyStart, msg.length), bc)
    val bh = Arc.sha256(body)
    val expected = scala.util.Try(Base64.getDecoder.decode(t("bh"))).getOrElse(Array.emptyByteArray)
    if !MessageDigest.isEqual(bh, expected) then return Some("ARC-Message-Signature の本文のハッシュが一致しません")
    val signed = new StringBuilder
    Arc.appendSelected(dkim, headers, names, hc, signed)
    signed.append(dkim.canonHeader(RawHeader(set.ams.name, Dkim.stripB(set.ams.raw)), hc).stripSuffix("\r\n"))
    checkSignature(algo, d, s, t("b"), signed.toString.getBytes(StandardCharsets.ISO_8859_1))
      .map(w => s"i=${set.i} の ARC-Message-Signature: $w")

  /** i=k の ARC-Seal の検証（1..k のセットを AAR・AMS・AS の順に relaxed で並べたものに対する署名）。 */
  private def verifySeal(sets: Vector[Arc.ArcSet], k: Int): Option[String] =
    val set = sets(k - 1)
    val t = dkim.tags(Arc.value(set.as))
    val d = t.getOrElse("d", "").toLowerCase
    val s = t.getOrElse("s", "")
    val algo = t.getOrElse("a", "").toLowerCase
    if d.isEmpty || s.isEmpty || !t.contains("b") || !t.contains("cv") then return Some(s"i=$k の ARC-Seal に必須のタグがありません")
    if t.contains("h") then return Some(s"i=$k の ARC-Seal に h= があります")
    if !Arc.Algorithms.contains(algo) then return Some(s"i=$k の ARC-Seal の未対応のアルゴリズム $algo")
    val data = Arc.sealData(dkim, sets.take(k - 1), set.aar, set.ams, RawHeader(set.as.name, Dkim.stripB(set.as.raw)))
    checkSignature(algo, d, s, t("b"), data).map(w => s"i=$k の ARC-Seal: $w")

  /** selector._domainkey.domain の公開鍵で署名を確かめる。正しければ None、だめなら理由。 */
  private def checkSignature(algo: String, d: String, s: String, b: String, data: Array[Byte]): Option[String] =
    dns.txt(s"$s._domainkey.$d") match
      case Left(e)    => Some(s"公開鍵を取得できません: $e")
      case Right(Nil) => Some("公開鍵がありません")
      case Right(txts) =>
        val kt = dkim.tags(txts.mkString)
        val p = kt.getOrElse("p", "")
        if p.isEmpty then return Some("鍵が失効しています")
        val ktype = kt.getOrElse("k", "rsa").toLowerCase
        if (ktype == "ed25519") != algo.startsWith("ed25519") then return Some("鍵の種類が違います")
        val ok = scala.util.Try {
          val sigBytes = Base64.getDecoder.decode(b)
          if ktype == "ed25519" then
            val v = Signature.getInstance("Ed25519")
            v.initVerify(Dkim.ed25519Public(Base64.getDecoder.decode(p)))
            v.update(Arc.sha256(data))
            v.verify(sigBytes)
          else
            val key = Dkim.rsaPublic(Base64.getDecoder.decode(p))
            if key.getModulus.bitLength < 1024 then false
            else
              val v = Signature.getInstance("SHA256withRSA")
              v.initVerify(key)
              v.update(data)
              v.verify(sigBytes)
        }
        ok match
          case scala.util.Success(true)  => None
          case scala.util.Success(false) => Some("署名が一致しません")
          case scala.util.Failure(e)     => Some(s"鍵または署名を読めません: ${e.getMessage}")

object Arc:
  val SealName = "ARC-Seal"
  val AmsName  = "ARC-Message-Signature"
  val AarName  = "ARC-Authentication-Results"

  /** ARC のセットの数の上限（RFC 8617 §4.2.1）。 */
  val MaxInstance = 50

  private[core] val Algorithms = Set("rsa-sha256", "ed25519-sha256")

  /** ARC-Message-Signature で署名するヘッダー（あるものだけ。From は必ず入れる）。 */
  val AmsHeaders = List("from", "to", "cc", "subject", "date", "message-id", "reply-to", "mime-version", "content-type")

  /** 同じ番号の ARC の3つのヘッダー。 */
  private[core] final case class ArcSet(i: Int, aar: RawHeader, ams: RawHeader, as: RawHeader)

  private[core] def isArc(name: String): Boolean =
    name.equalsIgnoreCase(SealName) || name.equalsIgnoreCase(AmsName) || name.equalsIgnoreCase(AarName)

  private[core] def value(h: RawHeader): String = h.raw.substring(h.raw.indexOf(':') + 1)

  /** ヘッダーの i= の値（AAR は先頭の i= だけを見る。結果の中の文字列に惑わされないように）。 */
  private[core] def instanceOf(dkim: Dkim, h: RawHeader): Option[Int] =
    val v = value(h)
    val raw =
      if h.name.equalsIgnoreCase(AarName) then
        val first = v.takeWhile(_ != ';')
        val eq = first.indexOf('=')
        Option.when(eq > 0 && first.substring(0, eq).trim.equalsIgnoreCase("i"))(first.substring(eq + 1).trim)
      else dkim.tags(v).get("i")
    raw.filter(s => s.nonEmpty && s.length <= 4 && s.forall(c => c >= '0' && c <= '9')).map(_.toInt).filter(_ >= 1)

  /** 今ある ARC のセットの最も大きい番号（なければ 0）。 */
  private[core] def highest(dkim: Dkim, arcHs: List[RawHeader]): Int =
    arcHs.flatMap(instanceOf(dkim, _)).maxOption.getOrElse(0)

  /** ARC のヘッダーを番号ごとのセットにまとめる（番号が 1..N で連続し、各番号に3種類が1つずつあること）。 */
  private[core] def collect(dkim: Dkim, arcHs: List[RawHeader]): Either[String, Vector[ArcSet]] =
    val withI = arcHs.map(h => h -> instanceOf(dkim, h))
    withI.collectFirst { case (h, None) => h } match
      case Some(h) => Left(s"${h.name} の i= が正しくありません")
      case None =>
        val grouped = withI.collect { case (h, Some(i)) => (i, h) }.groupBy(_._1)
        val n = grouped.keys.maxOption.getOrElse(0)
        if n == 0 then Right(Vector.empty)   // ARC のヘッダーがない
        else if n > MaxInstance then Left(s"ARC のセットが多すぎます（$n）")
        else if grouped.keySet != (1 to n).toSet then Left("ARC のセットの番号が連続していません")
        else
          val sets: IndexedSeq[Either[String, ArcSet]] = (1 to n).map { i =>
            val hs = grouped(i).map(_._2)
            def one(name: String): Option[RawHeader] = hs.filter(_.name.equalsIgnoreCase(name)) match
              case h :: Nil => Some(h)
              case _        => None
            (one(AarName), one(AmsName), one(SealName)) match
              case (Some(a), Some(m), Some(s)) => Right(ArcSet(i, a, m, s))
              case _                           => Left(s"i=$i の ARC のヘッダーがそろっていません（各1つずつ必要）")
          }
          sets.collectFirst { case Left(w) => w } match
            case Some(w) => Left(w)
            case None    => Right(sets.collect { case Right(s) => s }.toVector)

  /** h= のヘッダーを正規化して並べる（同じ名前は下から順に使う。DKIM と同じ）。 */
  private[core] def appendSelected(dkim: Dkim, headers: List[RawHeader], names: Seq[String], mode: String, out: StringBuilder): Unit =
    val used = scala.collection.mutable.Map.empty[String, Int]
    names.foreach { n =>
      val ln = n.toLowerCase
      val k = used.getOrElse(ln, 0)
      used(ln) = k + 1
      headers.filter(_.name.equalsIgnoreCase(ln)).reverse.lift(k).foreach(h => out.append(dkim.canonHeader(h, mode)))
    }

  /** ARC-Seal の署名の対象（RFC 8617 §5.1.1。前のセットを番号の順に AAR・AMS・AS と並べ、最後に今のセット。今の AS は b= を空にして末尾の CRLF を除く）。 */
  private[core] def sealData(dkim: Dkim, prior: Seq[ArcSet], aar: RawHeader, ams: RawHeader, asNoB: RawHeader): Array[Byte] =
    val sb = new StringBuilder
    def c(h: RawHeader) = dkim.canonHeader(h, "relaxed")
    prior.foreach { p => sb.append(c(p.aar)).append(c(p.ams)).append(c(p.as)) }
    sb.append(c(aar)).append(c(ams)).append(c(asNoB).stripSuffix("\r\n"))
    sb.toString.getBytes(StandardCharsets.ISO_8859_1)

  private[core] def sha256(data: Array[Byte]): Array[Byte] = MessageDigest.getInstance("SHA-256").digest(data)

  private def algorithmOf(key: PrivateKey): String = key.getAlgorithm match
    case "RSA"               => "rsa-sha256"
    case "Ed25519" | "EdDSA" => "ed25519-sha256"
    case other               => throw IllegalArgumentException(s"ARC の署名に使えない鍵です: $other")

  /** 署名して base64 にする（長いので折り返す）。ed25519 は RFC 8463 に合わせて SHA-256 のハッシュに署名する。 */
  private def signB64(algo: String, key: PrivateKey, data: Array[Byte]): String =
    val sig =
      if algo == "ed25519-sha256" then
        val s = Signature.getInstance("Ed25519")
        s.initSign(key)
        s.update(sha256(data))
        s.sign()
      else
        val s = Signature.getInstance("SHA256withRSA")
        s.initSign(key)
        s.update(data)
        s.sign()
    Base64.getEncoder.encodeToString(sig).grouped(72).mkString("\r\n\t ")

  /** ヘッダーに入れる文字列から改行を除く（ヘッダーの注入を防ぐ）。 */
  private def oneLine(s: String): String = s.replaceAll("[\r\n]+", " ").trim

  /** 転送するときに付ける ARC の3つのヘッダー（ARC-Seal・ARC-Message-Signature・ARC-Authentication-Results）を、先頭に付ける文字列として返す（CRLF 区切り・末尾 CRLF）。
    * authResults は、このサーバーが受信時に評価した結果（"spf=pass smtp.mailfrom=…; dkim=pass header.d=…; dmarc=pass header.from=…" の形、"mx.example.com;" の authserv-id は含めない）。
    * chain は verify の結果（cv の値を決める）。セットが上限（50）を超えるときは封印せず "" を返す。
    *
    * 簡略化: cv=fail のときは、壊れた鎖の前のセットは署名の対象に入れず、今のセットだけを封印する
    * （鎖はすでに失敗しており、受け取る側は cv=fail を見た時点で検証をやめるため。構造の壊れた鎖を並べられない場合にも困らない）。 */
  def seal(msg: Array[Byte], domain: String, selector: String, key: PrivateKey, authservId: String, authResults: String,
      chain: ArcResult, now: java.time.Instant): String =
    val dkim = new Dkim(NoDns)
    val (headers, bodyStart) = RawHeaders.split(msg)
    val arcHs = headers.filter(h => isArc(h.name))
    val i = highest(dkim, arcHs) + 1
    if i > MaxInstance then return ""
    val cv = if i == 1 then "none" else if chain.result == "pass" && chain.instance == i - 1 then "pass" else "fail"
    val algo = algorithmOf(key)
    val t = now.getEpochSecond
    // ARC-Authentication-Results
    val results = oneLine(authResults) match
      case "" => "none"
      case r  => r.replace("; ", ";\r\n\t")
    val aar = RawHeader(AarName, s"$AarName: i=$i; ${oneLine(authservId)};\r\n\t$results\r\n")
    // ARC-Message-Signature（relaxed/relaxed。ARC のヘッダーは署名しない）
    val body = dkim.canonBody(java.util.Arrays.copyOfRange(msg, bodyStart, msg.length), "relaxed")
    val bh = Base64.getEncoder.encodeToString(sha256(body))
    val present = AmsHeaders.filter(n => headers.exists(_.name.equalsIgnoreCase(n)))
    val names = if present.contains("from") then present else "from" :: present
    val signed = new StringBuilder
    appendSelected(dkim, headers, names, "relaxed", signed)
    val amsHead = s"i=$i; a=$algo; c=relaxed/relaxed; d=$domain; s=$selector; t=$t;\r\n\th=${names.mkString(":")};\r\n\tbh=$bh;\r\n\tb="
    signed.append(dkim.canonHeader(RawHeader(AmsName, s"$AmsName: $amsHead"), "relaxed").stripSuffix("\r\n"))
    val ams = RawHeader(AmsName, s"$AmsName: $amsHead${signB64(algo, key, signed.toString.getBytes(StandardCharsets.ISO_8859_1))}\r\n")
    // ARC-Seal（本文のハッシュはない。前のセットと今のセットを封印する）
    val asHead = s"i=$i; a=$algo; t=$t; cv=$cv; d=$domain; s=$selector;\r\n\tb="
    val prior = if cv == "fail" then Vector.empty else collect(dkim, arcHs).getOrElse(Vector.empty)
    val data = sealData(dkim, prior, aar, ams, RawHeader(SealName, s"$SealName: $asHead"))
    val as = s"$SealName: $asHead${signB64(algo, key, data)}\r\n"
    as + ams.raw + aar.raw

  private object NoDns extends Dns:
    def txt(name: String)       = Right(Nil)
    def addresses(name: String) = Right(Nil)
    def mx(name: String)        = Right(Nil)
    def ptr(ip: InetAddress)    = Right(Nil)
