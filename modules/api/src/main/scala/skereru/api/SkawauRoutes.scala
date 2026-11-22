package skereru.api

import cats.effect.*
import org.http4s.*
import skereru.core.ErrorCode
import skereru.shared.*

/**
 * skawau（ファイル共有）のファイルを選んで添付する・リンクを入れる、メールの添付を skawau に保存する.
 * 連携の手続き（connect・callback）は LinkRoutes. svc が None なら、このサーバーでは skawau と連携できない.
 */
final class SkawauRoutes(svc: Option[SkawauService], val sessions: SessionAuth) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "integrations" / "skawau" / "browse" => getBrowse(req)
    case req @ GET -> Root / "api" / "integrations" / "skawau" / "search" => getSearch(req)
    case req @ POST -> Root / "api" / "integrations" / "skawau" / "attach" => postAttach(req)
    case req @ POST -> Root / "api" / "integrations" / "skawau" / "links" => postLinks(req)
    case req @ POST -> Root / "api" / "integrations" / "skawau" / "save" => postSave(req)
  }

  private def getBrowse(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      withSvc(k =>
        param(req, "folder") match {
          case None => okJson(k.browse(s.user.id, None))
          case Some(f) =>
            scala.util.Try(java.util.UUID.fromString(f)).toOption
              .fold(err(ErrorCode.SkawauFileNotFound))(id => okJson(k.browse(s.user.id, Some(id))))
        }
      )
    )

  private def getSearch(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => withSvc(k => okJson(k.search(s.user.id, param(req, "q").getOrElse("")))))

  /** 添付にする（skereru の容量を使うので write）. */
  private def postAttach(req: Request[IO]): IO[Response[IO]] =
    authed(req, ApiScope.Write)(s => withSvc(k => body[SkawauAttachReq](req)(r => okJson(k.attach(s.user.id, r.ids)))))

  private def postLinks(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => withSvc(k => body[SkawauAttachReq](req)(r => okJson(k.links(s.user.id, r.ids)))))

  private def postSave(req: Request[IO]): IO[Response[IO]] =
    authed(req, ApiScope.Write)(s => withSvc(k => body[SkawauSaveReq](req)(r => okJson(k.save(s.user.id, r)))))

  private def withSvc(f: SkawauService => IO[Response[IO]]): IO[Response[IO]] =
    svc.fold(err(ErrorCode.SkawauNotConfigured))(f)
}
