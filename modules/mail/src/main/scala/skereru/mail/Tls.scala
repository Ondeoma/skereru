package skereru.mail

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.{KeyFactory, KeyStore, PrivateKey}
import java.security.cert.{CertificateFactory, X509Certificate}
import java.security.spec.PKCS8EncodedKeySpec
import java.util.Base64
import javax.net.ssl.{KeyManagerFactory, SSLContext, SSLSocket, TrustManager, X509TrustManager}
import scala.jdk.CollectionConverters.*

object Tls {

  /** TLS 1.2 以上・前方秘匿性のある AEAD の暗号だけを使う。 */
  def harden(
    s: SSLSocket
  ): Unit = {
    s.setEnabledProtocols(Array("TLSv1.3", "TLSv1.2").filter(s.getSupportedProtocols.contains))
    val strong = s.getSupportedCipherSuites.filter { c =>
      c.startsWith("TLS_AES_") || c.startsWith("TLS_CHACHA20_") ||
      (c.startsWith("TLS_ECDHE_") && (c.contains("_GCM_") || c.contains("CHACHA20")))
    }
    if strong.nonEmpty then s.setEnabledCipherSuites(strong)
    val p = s.getSSLParameters
    p.setUseCipherSuitesOrder(true)
    s.setSSLParameters(p)
  }

  private def blocks(
    pem: String,
    kind: String
  ): List[Array[Byte]] =
    s"(?s)-----BEGIN $kind-----(.*?)-----END $kind-----".r.findAllMatchIn(pem).map(m =>
      Base64.getMimeDecoder.decode(m.group(1))
    ).toList

  def fromPem(
    certPem: String,
    keyPem: String
  ): SSLContext = {
    val cf = CertificateFactory.getInstance("X.509")
    val chain = blocks(certPem, "CERTIFICATE").map(b =>
      cf.generateCertificate(new java.io.ByteArrayInputStream(b)).asInstanceOf[X509Certificate]
    )
    if chain.isEmpty then throw IllegalArgumentException("証明書（BEGIN CERTIFICATE）がありません")
    val key: PrivateKey =
      blocks(keyPem, "PRIVATE KEY").headOption.map(pkcs8)
        .orElse(blocks(keyPem, "RSA PRIVATE KEY").headOption.map(pkcs1 => pkcs8(wrapPkcs1(pkcs1))))
        .getOrElse(throw IllegalArgumentException(
          "秘密鍵は PKCS#8（BEGIN PRIVATE KEY）か PKCS#1（BEGIN RSA PRIVATE KEY）にしてください"
        ))
    val ks = KeyStore.getInstance("PKCS12")
    ks.load(null, null)
    ks.setKeyEntry("tls", key, Array.emptyCharArray, chain.toArray[java.security.cert.Certificate])
    context(ks, Array.emptyCharArray)
  }

  private def pkcs8(
    der: Array[Byte]
  ): PrivateKey =
    List("RSA", "EC", "Ed25519").view.flatMap(a =>
      scala.util.Try(KeyFactory.getInstance(a).generatePrivate(new PKCS8EncodedKeySpec(der))).toOption
    ).headOption
      .getOrElse(throw IllegalArgumentException("秘密鍵を読めません"))

  /** PKCS#1 の RSA 秘密鍵を PKCS#8 に包む。 */
  private def wrapPkcs1(
    pkcs1: Array[Byte]
  ): Array[Byte] = {
    def tag(
      t: Int,
      c: Array[Byte]
    ): Array[Byte] = {
      val len = c.length
      val l = if len < 128 then Array(len.toByte)
      else { val b = BigInt(len).toByteArray.dropWhile(_ == 0); Array((0x80 | b.length).toByte) ++ b }
      Array(t.toByte) ++ l ++ c
    }
    val algo = Array[Byte](
      0x30,
      0x0d,
      0x06,
      0x09,
      0x2a,
      0x86.toByte,
      0x48,
      0x86.toByte,
      0xf7.toByte,
      0x0d,
      0x01,
      0x01,
      0x01,
      0x05,
      0x00
    )
    tag(0x30, Array[Byte](0x02, 0x01, 0x00) ++ algo ++ tag(0x04, pkcs1))
  }

  private def context(
    ks: KeyStore,
    pass: Array[Char]
  ): SSLContext = {
    val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    kmf.init(ks, pass)
    val ctx = SSLContext.getInstance("TLS")
    ctx.init(kmf.getKeyManagers, null, null)
    ctx
  }

  /** 開発用の自己署名の証明書（JDK の keytool で作る）。 */
  def selfSigned(
    host: String
  ): SSLContext = {
    val dir = Files.createTempDirectory("skereru-tls")
    val ks = dir.resolve("self.p12")
    val keytool = Path.of(System.getProperty("java.home"), "bin", "keytool").toString
    val pb = new ProcessBuilder(
      keytool,
      "-genkeypair",
      "-alias",
      "tls",
      "-keyalg",
      "EC",
      "-groupname",
      "secp256r1",
      "-dname",
      s"CN=$host",
      "-validity",
      "3650",
      "-keystore",
      ks.toString,
      "-storetype",
      "PKCS12",
      "-storepass",
      "changeit",
      "-keypass",
      "changeit",
      "-ext",
      s"SAN=dns:$host,dns:localhost,ip:127.0.0.1"
    )
    pb.redirectErrorStream(true)
    val p = pb.start()
    val out = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    if p.waitFor() != 0 then throw IllegalStateException(s"自己署名の証明書を作れません: $out")
    val store = KeyStore.getInstance("PKCS12")
    val in = Files.newInputStream(ks)
    try store.load(in, "changeit".toCharArray)
    finally in.close()
    Files.deleteIfExists(ks); Files.deleteIfExists(dir)
    context(store, "changeit".toCharArray)
  }

  /** 証明書を確かめない（外への配送で、相手の証明書が自己署名のときに使う。暗号化はする）。 */
  lazy val trustAll: SSLContext = {
    val tm = new X509TrustManager {
      def checkClientTrusted(
        c: Array[X509Certificate],
        a: String
      ): Unit = ()
      def checkServerTrusted(
        c: Array[X509Certificate],
        a: String
      ): Unit = ()
      def getAcceptedIssuers: Array[X509Certificate] = Array.empty
    }
    val ctx = SSLContext.getInstance("TLS")
    ctx.init(null, Array[TrustManager](tm), null)
    ctx
  }

  /** 証明書の SAN（確認用）。 */
  def names(
    c: X509Certificate
  ): List[String] =
    Option(c.getSubjectAlternativeNames).toList.flatMap(_.asScala.toList.map(_.get(1).toString))
}
