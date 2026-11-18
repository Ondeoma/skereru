package skereru.api

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skereru.db.AvatarStore
import skereru.shared.AvatarImage

/** 自分のアイコンの保存・削除と、アイコンの画像. 保存しなければ IdP（sruru など）の画像を使う. */
final class AvatarRoutes(avatars: AvatarStore, val sessions: SessionAuth) extends RouteSupport:

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // 画像そのもの（PNG・JPEG・GIF・WebP、1MB まで）を本文にして送る. 種類は先頭のバイトで確かめる
    case req @ PUT -> Root / "api" / "me" / "avatar" =>
      authed(req) { s =>
        req.body.take(AvatarImage.MaxBytes + 1L).compile.to(Array).flatMap { data =>
          if data.length > AvatarImage.MaxBytes then err(Status.PayloadTooLarge, "画像は1MBまでにしてください")
          else AvatarImage.detect(data).fold(err(Status.UnsupportedMediaType, "PNG・JPEG・GIF・WebP の画像を選んでください")) { ct =>
            avatars.save(s.user.id, ct, data) *> NoContent()
          }
        }
      }

    case req @ DELETE -> Root / "api" / "me" / "avatar" =>
      authed(req)(s => avatars.delete(s.user.id) *> NoContent())

    // URL の v（版）が変われば別の画像なので、長くキャッシュしてよい
    case req @ GET -> Root / "api" / "users" / UuidVar(id) / "avatar" =>
      authed(req) { _ =>
        avatars.find(id).flatMap {
          case None => err(Status.NotFound, "アイコンがありません")
          case Some((ct, data)) =>
            // JSON の変換が優先されないよう、バイト列のまま返す
            IO.pure(Response[IO](Status.Ok).withBodyStream(fs2.Stream.emits(data)).putHeaders(
              headers.`Content-Type`(MediaType.unsafeParse(ct)), "Cache-Control" -> "private, max-age=31536000, immutable",
              "X-Content-Type-Options" -> "nosniff", "Content-Security-Policy" -> "default-src 'none'"))
        }
      }
  }
