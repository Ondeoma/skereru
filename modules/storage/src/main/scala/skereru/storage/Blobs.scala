package skereru.storage

import cats.effect.*

/**
 * メッセージの本体の保存（内容から決まる ID で重複をまとめる）。
 * 保存時に APP_SECRET から作った鍵で暗号化する（DB やストレージが漏れても中身は読めない）。
 */
trait Blobs {

  /** 保存して ID を返す。 */
  def put(
    bytes: Array[Byte]
  ): IO[String]
  def get(
    id: String
  ): IO[Option[Array[Byte]]]

  /** 使われなくなった本体を消す（定期処理）。 */
  def purge(): IO[Int]
}
