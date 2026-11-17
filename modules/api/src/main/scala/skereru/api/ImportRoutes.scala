package skereru.api

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

/** ほかのサービス（Gmail など）からのメールの取り込み。
  * ほかのサービスのパスワードを預かる・大量のメールを書き込むので、画面（Cookie のセッション）からだけ受け付ける。 */
final class ImportRoutes(importer: Importer, store: ImportStore, mail: MailStore, blobs: Blobs, val sessions: SessionAuth) extends RouteSupport:

  /** メールボックスのメッセージを mbox（mboxrd）で流す。ページごとに読むので、大きくてもメモリを使い切らない。 */
  private def mbox(boxes: List[Mailbox]): Stream[IO, Byte] =
    Stream.emits(boxes).flatMap { b =>
      Stream.unfoldLoopEval(0L)(after => mail.exportPage(b.id, after, 200).map(rows => (rows, rows.lastOption.map(_._1).filter(_ => rows.size == 200))))
        .flatMap(Stream.emits)
        .evalMap { (_, blobId, flags, date) =>
          blobs.get(blobId).map(_.map(raw => MailExport.entry(raw, MailExport.labels(b.specialUse, b.name, flags.contains(Flag.Seen), flags.contains(Flag.Flagged)), date)))
        }
        .flatMap(o => Stream.chunk(fs2.Chunk.array(o.getOrElse(Array.emptyByteArray))))
    }

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // 書き出し（mbox）。mailboxId がなければすべてのメールボックス。フォルダ・スター・未読は X-Gmail-Labels に書く（取り込みで戻せる）
    case req @ GET -> Root / "api" / "export" =>
      sessionOnly(req)(s => mail.mailboxes(s.user.id).flatMap { all =>
        val id = param(req, "mailboxId")
        val boxes = id.fold(all)(i => all.filter(_.id.toString == i))
        if id.isDefined && boxes.isEmpty then err(Status.NotFound, "メールボックスが見つかりません")
        else
          val day = java.time.LocalDate.now().toString
          val name = boxes match
            case List(b) if id.isDefined => s"skereru-${b.specialUse.getOrElse("folder")}-$day.mbox"
            case _                       => s"skereru-$day.mbox"
          IO.pure(Response[IO](Status.Ok)
            .withContentType(`Content-Type`(MediaType.unsafeParse("application/mbox")))
            .putHeaders(`Content-Disposition`("attachment", Map(ci"filename" -> name)))
            .withBodyStream(mbox(boxes)))
      })

    case req @ GET -> Root / "api" / "imports" =>
      sessionOnly(req)(s => store.list(s.user.id).flatMap(Ok(_)))

    // IMAP（Gmail なら imap.gmail.com:993 とアプリ パスワード）。ログインできるか確かめてから、裏で取り込む
    case req @ POST -> Root / "api" / "imports" / "imap" =>
      sessionOnly(req)(s => body[ImapImportReq](req) { r =>
        importer.startImap(s.user.id, ImapSource(r.host, r.port, r.user, r.password)).flatMap {
          case Right(job) => Created(job)
          case Left(e)    => err(Status.BadRequest, e)
        }
      })

    // ファイル（mbox・.eml）。画面がファイルを分けて /messages に送り、最後に /finish を呼ぶ
    case req @ POST -> Root / "api" / "imports" / "file" =>
      sessionOnly(req)(s => body[FileImportReq](req) { r =>
        val name = r.name.trim.filterNot(_ < 0x20).take(200)
        if name.isEmpty then err(Status.BadRequest, "ファイル名がありません")
        else store.create(s.user.id, "file", name, None, "running").flatMap {
          case Right(job) => Created(job)
          case Left(e)    => err(Status.Conflict, e.message)
        }
      })

    // 本文は [4バイトの長さ][メッセージ] の並び（application/octet-stream）。folder はラベルのないメッセージの入れ先、
    // tooLarge は画面が大きすぎて送らなかった数
    case req @ POST -> Root / "api" / "imports" / UuidVar(id) / "messages" =>
      sessionOnly(req)(s => req.body.compile.to(Array).flatMap { bytes =>
        importer.importBatch(s.user.id, id, bytes, param(req, "folder").getOrElse("取り込み"), intParam(req, "tooLarge", 0, 10_000)).flatMap {
          case Right(r) => Ok(r)
          case Left(e)  => err(Status.BadRequest, e)
        }
      })

    case req @ POST -> Root / "api" / "imports" / UuidVar(id) / "finish" =>
      sessionOnly(req)(s => store.find(s.user.id, id).flatMap {
        case Some(j) if j.kind == "file" => store.finish(id, "done", None) *> Ok(UpdatedDto(1))
        case _                           => err(Status.NotFound, "取り込みが見つかりません")
      })

    case req @ POST -> Root / "api" / "imports" / UuidVar(id) / "cancel" =>
      sessionOnly(req)(s => store.cancel(s.user.id, id).flatMap(n => Ok(UpdatedDto(n))))
  }
