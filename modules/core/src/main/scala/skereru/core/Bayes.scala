package skereru.core

/** ベイズ分類（トークンの数え上げと、Robinson-Fisher の結合）。 */
object Bayes {

  /** 学習・判定に使うトークン（件名・本文の語、日本語は2文字ずつ、URL のホスト、送信者のドメイン）。 */
  def tokens(
    msg: Array[Byte],
    root: MimePart
  ): Set[String] = {
    val subject = root.headerDecoded("Subject").getOrElse("")
    val from = root.header("From").flatMap(Address.parseOne)
    val texts = root.leaves().map(_._2).filter(_.mainType == "text").take(3).map { p =>
      val t = Mime.text(msg, p)
      if p.subType == "html" then Html.toText(t) else t
    }.mkString("\n").take(100_000)
    val out = scala.collection.mutable.Set.empty[String]
    def words(
      s: String,
      prefix: String
    ): Unit = {
      // 英数字の語（3〜20文字）
      """[\p{L}\p{N}][\p{L}\p{N}'_.-]*""".r.findAllIn(s.toLowerCase).foreach { w =>
        val ascii = w.filter(_ < 0x80)
        if ascii.length == w.length && w.length >= 3 && w.length <= 20 then out += prefix + w
      }
      // 日本語・中国語などは2文字ずつ
      val cjk = s.filter(c =>
        Character.UnicodeScript.of(c.toInt) match {
          case Character.UnicodeScript.HAN | Character.UnicodeScript.HIRAGANA | Character.UnicodeScript.KATAKANA | Character.UnicodeScript.HANGUL =>
            true
          case _ => c == '\n'
        }
      )
      cjk.split('\n').foreach(seg => seg.sliding(2).filter(_.length == 2).foreach(b => out += prefix + b))
    }
    words(subject, "s:")
    words(texts, "")
    Urls.find(texts).flatMap(Urls.host).foreach(h => out += "url:" + h)
    from.foreach(f => out += "from:" + f.domain)
    root.header("X-Mailer").orElse(root.header("User-Agent")).foreach(m =>
      out += "ua:" + m.toLowerCase.takeWhile(c => c != '/' && c != ' ').take(30)
    )
    out.toSet.take(3000)
  }

  /** トークンごとの数（spam・ham）と学習した通数から、迷惑メールである確率を出す。学習が足りなければ None。 */
  def classify(
    counts: Map[String, (Int, Int)],
    nSpam: Int,
    nHam: Int
  ): Option[Double] =
    if nSpam < 30 || nHam < 30 then None
    else {
      val s = 1.0; val x = 0.5
      val fs = counts.toList.flatMap {
        (
          _,
          sh
        ) =>
          {
            val (sp, hm) = sh
            val n = sp + hm
            if n == 0 then None
            else {
              val ps = sp.toDouble / nSpam; val ph = hm.toDouble / nHam
              val pw = if ps + ph == 0 then 0.5 else ps / (ps + ph)
              Some((s * x + n * pw) / (s + n))
            }
          }
      }.filter(f => math.abs(f - 0.5) > 0.1).sortBy(f => -math.abs(f - 0.5)).take(150)
      if fs.isEmpty then Some(0.5)
      else {
        val n = fs.size
        val h = chi2Q(-2 * fs.map(f => math.log(math.max(1e-200, 1 - f))).sum, 2 * n)
        val sp = chi2Q(-2 * fs.map(f => math.log(math.max(1e-200, f))).sum, 2 * n)
        // H が 1 に近いほど迷惑メールらしい（Robinson の指標）
        Some((1 + (1 - h) - (1 - sp)) / 2)
      }
    }

  /** カイ二乗分布の上側確率（自由度は偶数）。 */
  private def chi2Q(
    x2: Double,
    v: Int
  ): Double = {
    val m = x2 / 2
    var sum = math.exp(-m)
    var term = sum
    var i = 1
    while i < v / 2 do {
      term *= m / i
      sum += term
      i += 1
    }
    math.min(sum, 1.0)
  }
}
