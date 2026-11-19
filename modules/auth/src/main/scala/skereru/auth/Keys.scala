package skereru.auth

import sbirds.auth.Crypto

import java.security.SecureRandom

/** skereru の APIキー（skr_ で始まる）. */
object ApiKeys extends sbirds.auth.ApiKeys("skr_")

/** メールソフト用のアプリパスワード（16文字の英小文字を4文字ずつ区切ったもの。約75ビット）。DB にはハッシュだけを保存する。 */
object AppPasswords {
  private val rnd = new SecureRandom()
  private val alphabet = "abcdefghijkmnopqrstuvwxyz" // l は 1 と紛らわしいので除く
  def generate(): (String, String) = {
    val raw = (1 to 16).map(_ => alphabet(rnd.nextInt(alphabet.length))).mkString
    val shown = raw.grouped(4).mkString(" ")
    (shown, raw.take(4))
  }

  /** 入力の揺れ（空白・大文字）を除いてハッシュする。 */
  def hash(
    input: String
  ): Array[Byte] = Crypto.sha256("skereru-app-password:" + input.replaceAll("\\s", "").toLowerCase)
}

/** skereru の秘密の値の暗号化（鍵の salt は skereru 固有. 変えると保存済みの値を復号できなくなる）. */
object SecretBox {
  def apply(
    secret: String
  ): sbirds.auth.SecretBox = new sbirds.auth.SecretBox(secret, "skereru-secret-box:")
}

type SecretBox = sbirds.auth.SecretBox
