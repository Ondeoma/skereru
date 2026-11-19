package skereru.api

import java.util.UUID

import skereru.core.ErrorCode

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import fs2.Stream
import org.http4s.headers.{`Content-Disposition`, `Content-Type`}
import org.typelevel.ci.CIStringSyntax
import skereru.core.MailExport
import skereru.db.*
import skereru.mail.{ImapSource, Importer}
import skereru.shared.*
import skereru.storage.Blobs

/**
 * ほかのサービス（Gmail など）からのメールの取り込み。
 * ほかのサービスのパスワードを預かる・大量のメールを書き込むので、画面（Cookie のセッション）からだけ受け付ける。
 */
final class ImportRoutes(
  importer: Importer,
  store: ImportStore,
  mail: MailStore,
  blobs: Blobs,
  val sessions: SessionAuth) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // 書き出し（mbox）。mailboxId がなければすべてのメールボックス。フォルダ・スター・未読は X-Gmail-Labels に書く（取り込みで戻せる）
    case req @ GET -> Root / "api" / "export" => getExport(req)
    case req @ GET -> Root / "api" / "imports" => getImports(req)
    case req @ POST -> Root / "api" / "imports" / "imap" => postImportsImap(req)
    case req @ POST -> Root / "api" / "imports" / "file" => postImportsFile(req)
    case req @ POST -> Root / "api" / "imports" / UuidVar(id) / "messages" => postImportMessages(req, id)
    case req @ POST -> Root / "api" / "imports" / UuidVar(id) / "finish" => postImportFinish(req, id)
    case req @ POST -> Root / "api" / "imports" / UuidVar(id) / "cancel" => postImportCancel(req, id)
  }

  private def getExport(
    req: Request[IO]
  ): IO[Response[IO]] =
    sessionOnly(req)(s =>
      mail.mailboxes(s.user.id).flatMap { all =>
        val id = param(req, "mailboxId")
        val boxes = id.fold(all)(i => all.filter(_.id.toString == i))
        if id.isDefined && boxes.isEmpty then err(ErrorCode.MailboxNotFound)
        else {
          val day = java.time.LocalDate.now().toString
          val name = boxes match {
            case List(b) if id.isDefined => s"skereru-${b.specialUse.getOrElse("folder")}-$day.mbox"
            case _ => s"skereru-$day.mbox"
          }
          IO.pure(Response[IO](Status.Ok)
            .withContentType(`Content-Type`(MediaType.unsafeParse("application/mbox")))
            .putHeaders(`Content-Disposition`("attachment", Map(ci"filename" -> name)))
            .withBodyStream(mbox(boxes)))
        }
      }
    )

  private def getImports(
    req: Request[IO]
  ): IO[Response[IO]] =
    sessionOnly(req)(s => store.list(s.user.id).flatMap(Ok(_)))

  private def postImportsImap(
    req: Request[IO]
  ): IO[Response[IO]] =
    sessionOnly(req)(s =>
      body[ImapImportReq](req) { r =>
        importer.startImap(s.user.id, ImapSource(r.host, r.port, r.user, r.password)).flatMap {
          case Right(job) => Created(job)
          case Left(e) => err(ErrorCode.ImapImportInvalid, e)
        }
      }
    )

  private def postImportsFile(
    req: Request[IO]
  ): IO[Response[IO]] =
    sessionOnly(req)(s =>
      body[FileImportReq](req) { r =>
        val name = r.name.trim.filterNot(_ < 0x20).take(200)
        if name.isEmpty then err(ErrorCode.ImportFileNameMissing)
        else
          store.create(s.user.id, "file", name, None, "running").flatMap {
            case Right(job) => Created(job)
            case Left(e) => fail(e)
          }
      }
    )

  private def postImportMessages(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    sessionOnly(req)(s =>
      req.body.compile.to(Array).flatMap { bytes =>
        importer.importBatch(
          s.user.id,
          id,
          bytes,
          param(req, "folder").getOrElse("取り込み"),
          intParam(req, "tooLarge", 0, 10_000)
        ).flatMap {
          case Right(r) => Ok(r)
          case Left(e) => err(ErrorCode.FileImportInvalid, e)
        }
      }
    )

  private def postImportFinish(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    sessionOnly(req)(s =>
      store.find(s.user.id, id).flatMap {
        case Some(j) if j.kind == "file" => store.finish(id, "done", None) *> Ok(UpdatedDto(1))
        case _ => err(ErrorCode.ImportNotFound)
      }
    )

  private def postImportCancel(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    sessionOnly(req)(s => store.cancel(s.user.id, id).flatMap(n => Ok(UpdatedDto(n))))

  /** メールボックスのメッセージを mbox（mboxrd）で流す。ページごとに読むので、大きくてもメモリを使い切らない。 */
  private def mbox(
    boxes: List[Mailbox]
  ): Stream[IO, Byte] =
    Stream.emits(boxes).flatMap { b =>
      Stream.unfoldLoopEval(0L)(after =>
        mail.exportPage(b.id, after, 200).map(rows => (rows, rows.lastOption.map(_._1).filter(_ => rows.size == 200)))
      )
        .flatMap(Stream.emits)
        .evalMap {
          (
            _,
            blobId,
            flags,
            date
          ) =>
            blobs.get(blobId).map(_.map(raw =>
              MailExport.entry(
                raw,
                MailExport.labels(b.specialUse, b.name, flags.contains(Flag.Seen), flags.contains(Flag.Flagged)),
                date
              )
            ))
        }
        .flatMap(o => Stream.chunk(fs2.Chunk.array(o.getOrElse(Array.emptyByteArray))))
    }

}
