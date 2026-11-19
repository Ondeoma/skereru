package skereru.storage

import cats.effect.*
import cats.syntax.all.*
import skereru.db.MailStore

/** PostgreSQL に置く（既定。冗長化は Patroni に任せる）。 */
final class PgBlobs(
  store: MailStore,
  crypto: BlobCrypto,
  cache: BlobCache) extends Blobs {
  def put(
    bytes: Array[Byte]
  ): IO[String] = {
    val id = crypto.id(bytes)
    store.putBlob(id, bytes.length.toLong, Some(crypto.seal(bytes))).as(id)
  }

  def get(
    id: String
  ): IO[Option[Array[Byte]]] =
    cache.get(id) match {
      case Some(b) => IO.pure(Some(b))
      case None =>
        store.blobData(id).map(_.flatten.map(crypto.open(id, _))).flatTap(_.traverse_(b => IO(cache.put(id, b))))
    }

  def purge(): IO[Int] = store.purgeBlobs(1000).map(_.size)
}
