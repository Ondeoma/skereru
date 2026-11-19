package skereru.storage

import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, SecureRandom}
import javax.crypto.{Cipher, Mac}
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

/** 暗号化（AES-256-GCM）と ID（HMAC-SHA256。内容の推測に使えないよう秘密の鍵を使う）。 */
final class BlobCrypto(
  secret: String,
  enabled: Boolean) {
  private def derive(
    label: String
  ) = MessageDigest.getInstance("SHA-256").digest(s"skereru-$label:$secret".getBytes(StandardCharsets.UTF_8))
  private val encKey = new SecretKeySpec(derive("blob-enc"), "AES")
  private val idKey = new SecretKeySpec(derive("blob-id"), "HmacSHA256")
  private val rnd = new SecureRandom()
  private val Magic = Array[Byte]('S', 'K', '1')

  def id(
    bytes: Array[Byte]
  ): String = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(idKey)
    mac.doFinal(bytes).map(b => f"${b & 0xff}%02x").mkString
  }

  def seal(
    plain: Array[Byte]
  ): Array[Byte] =
    if !enabled then plain
    else {
      val iv = new Array[Byte](12)
      rnd.nextBytes(iv)
      val c = Cipher.getInstance("AES/GCM/NoPadding")
      c.init(Cipher.ENCRYPT_MODE, encKey, new GCMParameterSpec(128, iv))
      Magic ++ iv ++ c.doFinal(plain)
    }

  /**
   * 改ざんされていれば例外。
   * 暗号化が有効なら、暗号化されていないデータは受け付けない（ストレージに書ける攻撃者に、任意の本文を差し込ませない）。
   * 中身から ID を計算し直して照合する（ほかの ID の暗号文と入れ替えられていないか）。
   */
  def open(
    id: String,
    data: Array[Byte]
  ): Array[Byte] = {
    val plain =
      if data.length > 15 && data.take(3).sameElements(Magic) then {
        val c = Cipher.getInstance("AES/GCM/NoPadding")
        c.init(Cipher.DECRYPT_MODE, encKey, new GCMParameterSpec(128, data.slice(3, 15)))
        c.doFinal(data, 15, data.length - 15)
      } else if enabled then throw new SecurityException(s"blob $id is not encrypted")
      else data
    if !MessageDigest.isEqual(
        this.id(plain).getBytes(StandardCharsets.US_ASCII),
        id.getBytes(StandardCharsets.US_ASCII)
      )
    then
      throw new SecurityException(s"blob $id does not match its content")
    plain
  }
}
