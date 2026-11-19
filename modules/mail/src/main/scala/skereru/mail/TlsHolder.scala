package skereru.mail

import java.nio.file.{Files, Path}
import javax.net.ssl.SSLContext

/** TLS の証明書（PEM）。ファイルが更新されたら読み直す（Let's Encrypt の更新に追従する）。 */
final class TlsHolder(
  certPath: Option[Path],
  keyPath: Option[Path],
  hostname: String,
  log: String => Unit) {
  @volatile private var ctx: SSLContext = load()
  @volatile private var stamp: Long = modified

  private def modified: Long = (certPath.toList ++ keyPath.toList).map(p =>
    scala.util.Try(Files.getLastModifiedTime(p).toMillis).getOrElse(0L)
  ).sum

  private def load(): SSLContext = (certPath, keyPath) match {
    case (Some(c), Some(k)) => Tls.fromPem(Files.readString(c), Files.readString(k))
    case _ =>
      log(s"[tls] WARNING: TLS_CERT・TLS_KEY が未設定のため、自己署名の証明書（$hostname）を使います。本番では正しい証明書を設定してください")
      Tls.selfSigned(hostname)
  }

  def context: SSLContext = ctx

  /** 証明書のファイルが変わっていれば読み直す（定期的に呼ぶ）。 */
  def reloadIfChanged(): Unit = {
    val m = modified
    if m != stamp then
      try {
        ctx = load(); stamp = m
        log("[tls] 証明書を読み直しました")
      } catch case e: Throwable => log(s"[tls] 証明書を読み直せません: $e")
  }
}
