package skereru.storage

import cats.effect.*
import cats.syntax.all.*
import skereru.db.MailStore
import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.core.sync.RequestBody
import software.amazon.awssdk.http.apache.ApacheHttpClient
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.{S3Client, S3Configuration}
import software.amazon.awssdk.services.s3.model.*

import java.net.URI
import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, SecureRandom}
import javax.crypto.{Cipher, Mac}
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

final case class S3Config(endpoint: String, region: String, bucket: String, accessKey: String, secretKey: String, pathStyle: Boolean = true)

/** メッセージの本体の保存（内容から決まる ID で重複をまとめる）。
  * 保存時に APP_SECRET から作った鍵で暗号化する（DB やストレージが漏れても中身は読めない）。 */
trait Blobs:
  /** 保存して ID を返す。 */
  def put(bytes: Array[Byte]): IO[String]
  def get(id: String): IO[Option[Array[Byte]]]
  /** 使われなくなった本体を消す（定期処理）。 */
  def purge(): IO[Int]

/** 暗号化（AES-256-GCM）と ID（HMAC-SHA256。内容の推測に使えないよう秘密の鍵を使う）。 */
final class BlobCrypto(secret: String, enabled: Boolean):
  private def derive(label: String) = MessageDigest.getInstance("SHA-256").digest(s"skereru-$label:$secret".getBytes(StandardCharsets.UTF_8))
  private val encKey = new SecretKeySpec(derive("blob-enc"), "AES")
  private val idKey  = new SecretKeySpec(derive("blob-id"), "HmacSHA256")
  private val rnd    = new SecureRandom()
  private val Magic  = Array[Byte]('S', 'K', '1')

  def id(bytes: Array[Byte]): String =
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(idKey)
    mac.doFinal(bytes).map(b => f"${b & 0xff}%02x").mkString

  def seal(plain: Array[Byte]): Array[Byte] =
    if !enabled then plain
    else
      val iv = new Array[Byte](12)
      rnd.nextBytes(iv)
      val c = Cipher.getInstance("AES/GCM/NoPadding")
      c.init(Cipher.ENCRYPT_MODE, encKey, new GCMParameterSpec(128, iv))
      Magic ++ iv ++ c.doFinal(plain)

  /** 改ざんされていれば例外。
    * 暗号化が有効なら、暗号化されていないデータは受け付けない（ストレージに書ける攻撃者に、任意の本文を差し込ませない）。
    * 中身から ID を計算し直して照合する（ほかの ID の暗号文と入れ替えられていないか）。 */
  def open(id: String, data: Array[Byte]): Array[Byte] =
    val plain =
      if data.length > 15 && data.take(3).sameElements(Magic) then
        val c = Cipher.getInstance("AES/GCM/NoPadding")
        c.init(Cipher.DECRYPT_MODE, encKey, new GCMParameterSpec(128, data.slice(3, 15)))
        c.doFinal(data, 15, data.length - 15)
      else if enabled then throw new SecurityException(s"blob $id is not encrypted")
      else data
    if !MessageDigest.isEqual(this.id(plain).getBytes(StandardCharsets.US_ASCII), id.getBytes(StandardCharsets.US_ASCII)) then
      throw new SecurityException(s"blob $id does not match its content")
    plain

/** 最近読んだ本体を覚えておく（IMAP は同じメッセージを何度も部分的に読む）。 */
final class BlobCache(maxBytes: Long):
  private val map = new java.util.LinkedHashMap[String, Array[Byte]](256, 0.75f, true)
  private var size = 0L
  def get(id: String): Option[Array[Byte]] = synchronized(Option(map.get(id)))
  def put(id: String, b: Array[Byte]): Unit = synchronized {
    if b.length <= maxBytes / 4 && !map.containsKey(id) then
      map.put(id, b); size += b.length
      val it = map.entrySet().iterator()
      while size > maxBytes && it.hasNext do
        val e = it.next(); size -= e.getValue.length; it.remove()
  }

/** PostgreSQL に置く（既定。冗長化は Patroni に任せる）。 */
final class PgBlobs(store: MailStore, crypto: BlobCrypto, cache: BlobCache) extends Blobs:
  def put(bytes: Array[Byte]): IO[String] =
    val id = crypto.id(bytes)
    store.putBlob(id, bytes.length.toLong, Some(crypto.seal(bytes))).as(id)

  def get(id: String): IO[Option[Array[Byte]]] =
    cache.get(id) match
      case Some(b) => IO.pure(Some(b))
      case None =>
        store.blobData(id).map(_.flatten.map(crypto.open(id, _))).flatTap(_.traverse_(b => IO(cache.put(id, b))))

  def purge(): IO[Int] = store.purgeBlobs(1000).map(_.size)

/** S3 互換ストレージに置く（大きな環境向け。DB には ID と大きさだけ）。 */
final class S3Blobs(client: S3Client, bucket: String, store: MailStore, crypto: BlobCrypto, cache: BlobCache) extends Blobs:
  private def key(id: String) = s"blobs/${id.take(2)}/$id"

  def put(bytes: Array[Byte]): IO[String] =
    val id = crypto.id(bytes)
    IO.blocking(client.putObject(PutObjectRequest.builder().bucket(bucket).key(key(id)).contentType("application/octet-stream").build(),
      RequestBody.fromBytes(crypto.seal(bytes)))) *> store.putBlob(id, bytes.length.toLong, None).as(id)

  def get(id: String): IO[Option[Array[Byte]]] =
    cache.get(id) match
      case Some(b) => IO.pure(Some(b))
      case None =>
        store.blobData(id).flatMap {
          case None             => IO.pure(None)
          case Some(Some(data)) => IO.pure(Some(crypto.open(id, data))) // DB に置いた古いもの
          case Some(None) =>
            IO.blocking(client.getObjectAsBytes(GetObjectRequest.builder().bucket(bucket).key(key(id)).build()).asByteArray())
              .map(b => Some(crypto.open(id, b))).recover { case _: NoSuchKeyException => None }
        }.flatTap(_.traverse_(b => IO(cache.put(id, b))))

  def purge(): IO[Int] =
    store.purgeBlobs(1000).flatTap(_.traverse_(id =>
      IO.blocking(client.deleteObject(DeleteObjectRequest.builder().bucket(bucket).key(key(id)).build())).attempt)).map(_.size)

  def ensureBucket: IO[Unit] =
    IO.blocking {
      try client.headBucket(HeadBucketRequest.builder().bucket(bucket).build())
      catch case _: NoSuchBucketException => client.createBucket(CreateBucketRequest.builder().bucket(bucket).build())
    }.void

object S3Blobs:
  def resource(cfg: S3Config, store: MailStore, crypto: BlobCrypto, cache: BlobCache): Resource[IO, S3Blobs] =
    Resource.fromAutoCloseable(IO.blocking {
      S3Client.builder()
        .endpointOverride(URI.create(cfg.endpoint))
        .region(Region.of(cfg.region))
        .credentialsProvider(StaticCredentialsProvider.create(AwsBasicCredentials.create(cfg.accessKey, cfg.secretKey)))
        .serviceConfiguration(S3Configuration.builder().pathStyleAccessEnabled(cfg.pathStyle).build())
        .httpClientBuilder(ApacheHttpClient.builder())
        .build()
    }).map(new S3Blobs(_, cfg.bucket, store, crypto, cache)).evalTap(_.ensureBucket)
