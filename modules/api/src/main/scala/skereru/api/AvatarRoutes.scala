package skereru.api

import java.util.UUID

import skereru.core.ErrorCode

import cats.effect.*
import org.http4s.*
import skereru.db.AvatarStore
import skereru.shared.AvatarImage

/** 自分のアイコンの保存・削除と、アイコンの画像. 保存しなければ IdP（sruru など）の画像を使う. */
final class AvatarRoutes(avatars: AvatarStore, val sessions: SessionAuth) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // 画像そのもの（PNG・JPEG・GIF・WebP、1MB まで）を本文にして送る. 種類は先頭のバイトで確かめる
    case req @ PUT -> Root / "api" / "me" / "avatar" => putMeAvatar(req)
    case req @ DELETE -> Root / "api" / "me" / "avatar" => deleteMeAvatar(req)
    case req @ GET -> Root / "api" / "users" / UuidVar(id) / "avatar" => getUserAvatar(req, id)
  }

  private def putMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req) { s =>
      req.body.take(AvatarImage.MaxBytes + 1L).compile.to(Array).flatMap { data =>
        if data.length > AvatarImage.MaxBytes then err(ErrorCode.AvatarTooLarge)
        else
          AvatarImage.detect(data).fold(err(ErrorCode.AvatarTypeUnsupported)) { ct =>
            avatars.save(s.user.id, ct, data) *> NoContent()
          }
      }
    }

  private def deleteMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => avatars.delete(s.user.id) *> NoContent())

  private def getUserAvatar(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req) { _ =>
      avatars.find(id).flatMap {
        case None => err(ErrorCode.AvatarNotFound)
        case Some((ct, data)) =>
          // JSON の変換が優先されないよう、バイト列のまま返す
          IO.pure(Response[IO](Status.Ok).withBodyStream(fs2.Stream.emits(data)).putHeaders(
            headers.`Content-Type`(MediaType.unsafeParse(ct)),
            "Cache-Control" -> "private, max-age=31536000, immutable",
            "X-Content-Type-Options" -> "nosniff",
            "Content-Security-Policy" -> "default-src 'none'"
          ))
      }
    }

}
