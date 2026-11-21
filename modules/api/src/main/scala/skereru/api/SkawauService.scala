package skereru.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.{Decoder, Json}
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.`Content-Type`
import sbirds.db.{OutboxResult, Periodic}
import sbirds.links.{LinkClient, LinkFailure}
import sbirds.shared.AppError
import skereru.core.ErrorCode
import skereru.db.*
import skereru.shared.*

import java.util.UUID
import scala.concurrent.duration.*

/**
 * skawau（ファイル共有）との連携. 本人が OAuth で連携したトークン（read・write）で、本人の権限で skawau を使う.
 *  - メールを作るとき: skawau のファイルを選んで添付する（中身を取ってきて、skereru のアップロードにする）か、URL を本文に入れる.
 *  - 受け取った・送ったメールの添付を skawau に保存する（画面から手で. 振り分けの規則で自動でも）.
 * 保存先の既定は、本人の skawau のルートの「メールの添付」フォルダ（なければ skawau が作る）. 同じ名前があれば名前を変えて両方残す.
 */
final class SkawauService(link: LinkClient, mailSvc: MailService, saves: SkawauSaveStore, maxUploadBytes: Long) {

  /** 既定の保存先（skawau の自分のルートの下）. */
  val DefaultFolder = "メールの添付"

  /** 一度に添付・保存できる数. */
  val MaxFiles = 20

  private given Decoder[SkawauItemDto] = Decoder.instance(c =>
    for {
      id <- c.get[UUID]("id")
      name <- c.get[String]("name")
      isDir <- c.getOrElse[Boolean]("isDir")(false)
      size <- c.getOrElse[Long]("size")(0L)
      ct <- c.getOrElse[Option[String]]("contentType")(None)
    } yield SkawauItemDto(id, name, isDir, size, ct)
  )

  /** フォルダの中身（None なら自分のルート）. */
  def browse(userId: UUID, folder: Option[UUID]): Res[SkawauFolderDto] =
    get(userId, folder.fold("/api/files/root")(id => s"/api/files/$id/children"))(j => folderOf(j))

  /** 名前で探す. */
  def search(userId: UUID, q: String): Res[List[SkawauItemDto]] =
    if q.trim.isEmpty then IO.pure(Right(Nil))
    else
      get(userId, "/api/search", Map("q" -> q.trim.take(200), "limit" -> "100"))(j =>
        j.asArray.map(_.toList.flatMap(_.hcursor.downField("node").as[SkawauItemDto].toOption))
      )

  /** skawau のファイルを、メールの添付（skereru のアップロード）にする. フォルダ・大きすぎるものは断る（リンクを入れてもらう）. */
  def attach(userId: UUID, ids: List[UUID]): Res[List[UploadDto]] =
    if ids.size > MaxFiles then IO.pure(Left(AppError(ErrorCode.SkawauTooMany)))
    else
      ids.distinct.foldLeftM[IO, Either[AppError, List[UploadDto]]](Right(Nil)) {
        case (Left(e), _) => IO.pure(Left(e))
        case (Right(acc), id) =>
          item(userId, id).flatMap {
            case Left(e) => IO.pure(Left(e))
            case Right(n) if n.isDir =>
              IO.pure(Left(AppError(ErrorCode.SkawauFolderNotAllowed, Some(s"「${n.name}」はフォルダです。リンクを入れてください"))))
            case Right(n) if n.size > maxUploadBytes => IO.pure(Left(tooLarge(n.name)))
            case Right(n) =>
              download(userId, id).flatMap {
                case Left(e) => IO.pure(Left(e))
                case Right(bytes) =>
                  mailSvc.upload(userId, n.name, n.contentType.getOrElse("application/octet-stream"), bytes)
                    .map(_.map(acc :+ _))
              }
          }
      }

  /** 本文に入れる URL（相手は skawau の権限で開く）. */
  def links(userId: UUID, ids: List[UUID]): Res[List[SkawauLinkDto]] =
    if ids.size > MaxFiles then IO.pure(Left(AppError(ErrorCode.SkawauTooMany)))
    else ids.distinct.traverse(id => item(userId, id).map(_.map(n => linkOf(n)))).map(_.sequence)

  /** メールの添付を skawau に保存する. */
  def save(userId: UUID, r: SkawauSaveReq): Res[SkawauSavedDto] =
    mailSvc.attachments(userId, r.messageId, r.parts.take(100)).flatMap {
      case Left(e) => IO.pure(Left(e))
      case Right(Nil) => IO.pure(Left(AppError(ErrorCode.SkawauNoAttachments)))
      case Right(as) if as.size > 100 => IO.pure(Left(AppError(ErrorCode.SkawauTooMany)))
      case Right(as) =>
        val target: Res[(UUID, List[String])] = r.folderId match {
          case Some(f) => IO.pure(Right(f -> Nil))
          case None => browse(userId, None).map(_.map(root => root.id -> List(DefaultFolder)))
        }
        target.flatMap {
          case Left(e) => IO.pure(Left(e))
          case Right((folder, rel)) =>
            as.foldLeftM[IO, Either[AppError, List[(SkawauLinkDto, Option[UUID])]]](Right(Nil)) {
              case (Left(e), _) => IO.pure(Left(e))
              case (Right(acc), (_, name, mime, bytes)) =>
                upload(userId, folder, rel, name, mime, bytes).map(_.map(acc :+ _))
            }.map(_.map { saved =>
              val parent = saved.flatMap(_._2).headOption
              SkawauSavedDto(saved.map(_._1), parent.map(p => s"${link.cfg.url}/files/$p"))
            })
        }
    }

  // ---------------------------------------------------------------- 振り分けの規則の「添付を skawau に保存」

  /** 保存待ちに積む（添付のないメールは、取り出したときに何もしないで終える）. */
  def enqueue(userId: UUID, messageId: UUID): IO[Unit] = saves.enqueue(userId, messageId)

  def lastFailure(userId: UUID): IO[Option[String]] = saves.lastFailure(userId)

  private def saveQueued(r: SkawauSaveRow): IO[OutboxResult] =
    save(r.userId, SkawauSaveReq(r.messageId, Nil, None)).map {
      case Right(_) => OutboxResult.Sent
      case Left(e) if e.code == ErrorCode.SkawauNoAttachments => OutboxResult.Sent
      case Left(e) if e.code == ErrorCode.SkawauRequestFailed => OutboxResult.Retry(e.message)
      case Left(e) => OutboxResult.Skip(e.message)
    }

  /** 保存待ちを1回分処理する（処理した数）. */
  def processQueue: IO[Int] = saves.process(20)(saveQueued)

  def loop: IO[Nothing] = Periodic.run(20.seconds, 10.seconds, ErrorCode.SkawauSaveWorkerFailed)(processQueue)

  def purgeLoop: IO[Nothing] = Periodic.run(1.hour, 24.hours, ErrorCode.SkawauSaveWorkerFailed)(saves.purge(14))

  // ---------------------------------------------------------------- skawau の API

  private def item(userId: UUID, id: UUID): Res[SkawauItemDto] =
    get(userId, s"/api/files/$id")(_.hcursor.downField("node").as[SkawauItemDto].toOption)

  private def linkOf(n: SkawauItemDto): SkawauLinkDto =
    SkawauLinkDto(n.id, n.name, s"${link.cfg.url}/${if n.isDir then "files" else "view"}/${n.id}", n.isDir)

  /** skawau のフォルダの中身. パンくずは今のフォルダの手前まで. ルート（名前が空）は skawau の画面と同じ「すべてのファイル」. */
  private def folderOf(j: Json): Option[SkawauFolderDto] = {
    val h = j.hcursor
    def named(n: String) = if n.isEmpty then "すべてのファイル" else n
    for {
      f <- h.downField("folder").as[SkawauItemDto].toOption
      path = h.downField("path").as[List[Json]].getOrElse(Nil).flatMap { c =>
        (c.hcursor.get[UUID]("id"), c.hcursor.get[String]("name")).tupled.toOption.map((i, n) =>
          SkawauItemDto(i, named(n), true, 0, None)
        )
      }.filterNot(_.id == f.id)
      items = h.downField("items").as[List[SkawauItemDto]].getOrElse(Nil).sortBy(i => (!i.isDir, i.name.toLowerCase))
    } yield SkawauFolderDto(f.id, named(f.name), path, items)
  }

  private def get[A](userId: UUID,
                     path: String,
                     query: Map[String, String] = Map.empty)(read: Json => Option[A]): Res[A] =
    link.call(userId, Request[IO](Method.GET, link.api(path).withQueryParams(query)))(_.as[Json]).map {
      case Right(j) => read(j).toRight(AppError(ErrorCode.SkawauRequestFailed, Some("skawau の応答を読めません")))
      case Left(f) => Left(failure(f))
    }

  /** 中身を取ってくる（大きすぎる応答は途中でやめる）. */
  private def download(userId: UUID, id: UUID): Res[Array[Byte]] =
    link.call(
      userId,
      Request[IO](Method.GET, link.api(s"/api/files/$id/content").withQueryParam("download", "1")),
      2.minutes
    )(_.body.take(maxUploadBytes + 1).compile.to(Array)).map {
      case Right(b) if b.length > maxUploadBytes => Left(tooLarge("ファイル"))
      case Right(b) => Right(b)
      case Left(f) => Left(failure(f))
    }

  /** フォルダ（の下の rel）に置く. 同じ名前があれば名前を変えて両方残す. (置いたファイル, 置いたフォルダ). */
  private def upload(userId: UUID,
                     folder: UUID,
                     rel: List[String],
                     name: String,
                     mime: String,
                     bytes: Array[Byte]): Res[(SkawauLinkDto, Option[UUID])] = {
    val q = Map("name" -> name, "conflict" -> "rename") ++
      (if rel.isEmpty then Map() else Map("path" -> rel.mkString("/")))
    val ct = MediaType.parse(mime).toOption.getOrElse(MediaType.application.`octet-stream`)
    val req = Request[IO](Method.PUT, link.api(s"/api/files/$folder/upload").withQueryParams(q))
      .withEntity(bytes)(using EntityEncoder.byteArrayEncoder) // CirceEntityCodec の JSON の配列にしない
      .withContentType(`Content-Type`(ct))
    link.call(userId, req, 2.minutes)(_.as[Json]).map {
      case Right(j) =>
        j.hcursor.as[SkawauItemDto].toOption
          .map(n => linkOf(n) -> j.hcursor.get[UUID]("parentId").toOption)
          .toRight(AppError(ErrorCode.SkawauRequestFailed, Some("skawau の応答を読めません")))
      case Left(f) => Left(failure(f))
    }
  }

  private def tooLarge(name: String): AppError =
    AppError(
      ErrorCode.SkawauFileTooLarge,
      Some(s"「$name」は大きすぎて添付できません（上限 ${maxUploadBytes / 1024 / 1024}MB）。リンクを入れてください")
    )

  private def failure(f: LinkFailure): AppError = f match {
    case LinkFailure.NotConnected => AppError(ErrorCode.SkawauNotConnected)
    case LinkFailure.Expired => AppError(ErrorCode.SkawauNotConnected, Some("skawau との連携が切れています。設定の「連携」で連携し直してください"))
    case LinkFailure.Rejected(404) | LinkFailure.Rejected(403) => AppError(ErrorCode.SkawauFileNotFound)
    case LinkFailure.Rejected(413) => AppError(ErrorCode.SkawauRequestFailed, Some("skawau の容量の上限を超えます"))
    case LinkFailure.Rejected(code) => AppError(ErrorCode.SkawauRequestFailed, Some(s"skawau が $code を返しました"))
    case LinkFailure.Unreachable(_) => AppError(ErrorCode.SkawauRequestFailed, Some("skawau に接続できませんでした"))
  }
}
