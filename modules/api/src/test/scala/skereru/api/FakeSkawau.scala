package skereru.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import org.http4s.headers.Authorization

import java.util.UUID

/**
 * 試験用の skawau（ルート（名前は空）に「見積書.pdf」が1つ）. トークン "skawau-token" だけを受け付ける.
 * アップロードされたもの（フォルダ・path・名前・中身）は uploads に貯める.
 */
final class FakeSkawau {
  val root: UUID = UUID.randomUUID()
  val file: UUID = UUID.randomUUID()
  val big: UUID = UUID.randomUUID()
  val mailFolder: UUID = UUID.randomUUID()
  val content: Array[Byte] = "%PDF-見積書".getBytes("UTF-8")
  val uploads = new java.util.concurrent.ConcurrentLinkedQueue[(UUID, Option[String], String, Array[Byte])]()

  private def node(id: UUID, name: String, isDir: Boolean, size: Long, parent: Option[UUID] = None) = Json.obj(
    "id" -> id.asJson,
    "parentId" -> parent.asJson,
    "name" -> name.asJson,
    "isDir" -> isDir.asJson,
    "size" -> size.asJson,
    "contentType" -> (if isDir then Json.Null else "application/pdf".asJson)
  )

  private val rootJson = Json.obj(
    "folder" -> node(root, "", true, 0),
    "path" -> Json.arr(),
    "items" -> Json.arr(node(file, "見積書.pdf", false, content.length.toLong), node(big, "大きい.mov", false, 1L << 40)),
    "inShare" -> false.asJson
  )

  private object Name extends OptionalQueryParamDecoderMatcher[String]("name")
  private object Path extends OptionalQueryParamDecoderMatcher[String]("path")

  val app: HttpApp[IO] = HttpRoutes.of[IO] {
    case req if !req.headers.get[Authorization].exists(_.credentials.renderString == "Bearer skawau-token") =>
      IO.pure(Response[IO](Status.Unauthorized))
    case GET -> Root / "api" / "files" / "root" => Ok(rootJson)
    case GET -> Root / "api" / "files" / UUIDVar(id) if id == file =>
      Ok(Json.obj("node" -> node(file, "見積書.pdf", false, content.length.toLong), "path" -> "/".asJson))
    case GET -> Root / "api" / "files" / UUIDVar(id) if id == big =>
      Ok(Json.obj("node" -> node(big, "大きい.mov", false, 1L << 40), "path" -> "/".asJson))
    case GET -> Root / "api" / "files" / UUIDVar(id) / "content" if id == file =>
      IO.pure(Response[IO](Status.Ok).withEntity(content)(using EntityEncoder.byteArrayEncoder))
    case req @ PUT -> Root / "api" / "files" / UUIDVar(folder) / "upload" :? Name(name) +& Path(path) =>
      req.body.compile.to(Array).flatMap { b =>
        IO(uploads.add((folder, path, name.getOrElse(""), b))) *>
          Created(node(UUID.randomUUID(), name.getOrElse(""), false, b.length.toLong, Some(mailFolder)))
      }
    case _ => NotFound()
  }.orNotFound
}

object FakeSkawau {
  val Url = "https://skawau.test"
}
