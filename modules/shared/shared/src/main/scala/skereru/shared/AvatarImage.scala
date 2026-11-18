package skereru.shared

/** 保存できるアイコン（画像の種類と大きさの上限）. SVG はスクリプトを含められるので受け付けない. */
object AvatarImage {
  val MaxBytes = 1024 * 1024
  /** 先頭のバイトから画像の種類を決める（PNG・JPEG・GIF・WebP 以外は None）. */
  def detect(b: Array[Byte]): Option[String] = {
    def at(i: Int, xs: Int*): Boolean = b.length >= i + xs.size && xs.zipWithIndex.forall((x, j) => (b(i + j) & 0xff) == x)
    if (at(0, 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)) Some("image/png")
    else if (at(0, 0xff, 0xd8, 0xff)) Some("image/jpeg")
    else if (at(0, 0x47, 0x49, 0x46, 0x38)) Some("image/gif")
    else if (at(0, 0x52, 0x49, 0x46, 0x46) && at(8, 0x57, 0x45, 0x42, 0x50)) Some("image/webp")
    else None
  }
}
