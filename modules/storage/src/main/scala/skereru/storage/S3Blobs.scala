package skereru.storage

import cats.effect.*
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import fs2.io.file.Files
import sbirds.storage.{BlobStore, S3BlobStore, S3Config}
import skereru.db.MailStore

/**
 * S3 互換ストレージに置く（大きな環境向け. DB には ID と大きさだけ）. 読み書きは sbirds-lib の S3BlobStore.
 * lib の put はファイルから送るので、暗号化した本体をいったん一時ファイルに書く.
 */
final class S3Blobs(
  files: BlobStore,
  store: MailStore,
  crypto: BlobCrypto,
  cache: BlobCache) extends Blobs {
  private def key(
    id: String
  ) = s"blobs/${id.take(2)}/$id"

  def put(
    bytes: Array[Byte]
  ): IO[String] = {
    val id = crypto.id(bytes)
    Files[IO].tempFile.use { tmp =>
      Stream.chunk(Chunk.array(crypto.seal(bytes))).through(Files[IO].writeAll(tmp)).compile.drain *>
        files.put(key(id), tmp, "application/octet-stream")
    } *> store.putBlob(id, bytes.length.toLong, None).as(id)
  }

  def get(
    id: String
  ): IO[Option[Array[Byte]]] =
    cache.get(id) match {
      case Some(b) => IO.pure(Some(b))
      case None =>
        store.blobData(id).flatMap {
          case None => IO.pure(None)
          case Some(Some(data)) => IO.pure(Some(crypto.open(id, data))) // DB に置いた古いもの
          case Some(None) =>
            files.get(key(id)).flatMap(_.traverse(_._2.compile.to(Array))).map(_.map(crypto.open(id, _)))
        }.flatTap(_.traverse_(b => IO(cache.put(id, b))))
    }

  def purge(): IO[Int] =
    store.purgeBlobs(1000).flatTap(_.traverse_(id => files.delete(key(id)).attempt)).map(_.size)
}

object S3Blobs {
  def resource(
    cfg: S3Config,
    store: MailStore,
    crypto: BlobCrypto,
    cache: BlobCache
  ): Resource[IO, S3Blobs] =
    S3BlobStore.resource(cfg).evalTap(_.ensureBucket).map(new S3Blobs(_, store, crypto, cache))
}
