package skereru.web

import io.circe.*
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** API の呼び出しのエラー. */
final case class ApiError(status: Int, message: String) extends Exception(message)

/** バックエンドの REST API. 401 はログイン画面へ、エラーは {"error": ...} の文言にする.
 * 更新系の要求には X-Skereru ヘッダーを付ける（サーバーはこれがない Cookie の更新を拒否する＝CSRF 対策）. */
object Api {

  private def request(method: dom.HttpMethod, url: String, body: Option[Json] = None): Future[Option[Json]] =
    raw(method, url, body.map(b => ("application/json", b.noSpaces)))

  /** body は (Content-Type, 本文). */
  private def raw(method: dom.HttpMethod, url: String, body: Option[(String, dom.BodyInit)]): Future[Option[Json]] = {
    val init = new dom.RequestInit {}
    init.method = method
    init.credentials = dom.RequestCredentials.`same-origin`
    val headers = js.Dictionary("X-Skereru" -> "1")
    body.foreach { (ct, b) =>
      headers("Content-Type") = ct
      init.body = b
    }
    init.headers = headers
    dom.fetch(url, init).toFuture.flatMap(handle)
  }

  private def handle(res: dom.Response): Future[Option[Json]] =
    if (res.status == 401) {
      val back = dom.window.location.pathname + dom.window.location.search
      if (!dom.window.location.pathname.startsWith("/login"))
        dom.window.location.href = "/auth/login?returnTo=" + js.URIUtils.encodeURIComponent(back)
      Future.failed(ApiError(401, "ログインしてください"))
    } else if (!res.ok) {
      res.text().toFuture.recover(_ => "").flatMap { t =>
        val msg = parse(t).toOption.flatMap(_.hcursor.get[String]("error").toOption).getOrElse(s"${res.status} ${res.statusText}")
        Future.failed(ApiError(res.status, msg))
      }
    } else if (res.status == 204) Future.successful(None)
    else res.text().toFuture.map(t => if (t.isEmpty) None else Some(parse(t).fold(throw _, identity)))

  private def as[A: Decoder](j: Option[Json]): A = j.getOrElse(Json.Null).as[A].fold(e => throw ApiError(0, e.getMessage), identity)

  def get[A: Decoder](url: String): Future[A] = request(dom.HttpMethod.GET, url).map(as[A])
  def post[A: Decoder](url: String, body: Json = Json.obj()): Future[A] = request(dom.HttpMethod.POST, url, Some(body)).map(as[A])
  def put[A: Decoder](url: String, body: Json): Future[A] = request(dom.HttpMethod.PUT, url, Some(body)).map(as[A])
  def patch[A: Decoder](url: String, body: Json): Future[A] = request(dom.HttpMethod.PATCH, url, Some(body)).map(as[A])
  def send(method: dom.HttpMethod, url: String, body: Option[Json] = None): Future[Unit] = request(method, url, body).map(_ => ())
  def delete(url: String): Future[Unit] = send(dom.HttpMethod.DELETE, url)
  def postUnit(url: String, body: Json = Json.obj()): Future[Unit] = send(dom.HttpMethod.POST, url, Some(body))
  def putUnit(url: String, body: Json): Future[Unit] = send(dom.HttpMethod.PUT, url, Some(body))
  def patchUnit(url: String, body: Json): Future[Unit] = send(dom.HttpMethod.PATCH, url, Some(body))

  /** バイナリの本文を送る（application/octet-stream）. */
  def postBytes[A: Decoder](url: String, bytes: js.typedarray.Uint8Array): Future[A] =
    raw(dom.HttpMethod.POST, url, Some(("application/octet-stream", bytes.asInstanceOf[dom.BodyInit]))).map(as[A])

  /** 画像などをそのまま（Content-Type はファイルの種類）PUT で送る. */
  def putBlob(url: String, data: dom.Blob): Future[Unit] =
    raw(dom.HttpMethod.PUT, url, Some((data.`type`, data))).map(_ => ())

  def enc(s: String): String = js.URIUtils.encodeURIComponent(s)

  def query(params: (String, Option[String])*): String = {
    val q = params.collect { case (k, Some(v)) if v.nonEmpty => s"$k=${enc(v)}" }
    if (q.isEmpty) "" else q.mkString("?", "&", "")
  }

  /** ファイルを multipart/form-data（file）で送る. */
  def upload(file: dom.File): Future[UploadDto] = {
    val fd = new dom.FormData()
    fd.append("file", file, file.name)
    val init = new dom.RequestInit {}
    init.method = dom.HttpMethod.POST
    init.credentials = dom.RequestCredentials.`same-origin`
    init.headers = js.Dictionary("X-Skereru" -> "1")
    init.body = fd
    dom.fetch("/api/uploads", init).toFuture.flatMap(handle).map(as[UploadDto])
  }

  // ---------------------------------------------------------------- よく使うもの

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")
  /** ログアウトして、次に開く URL（IdP のログアウト画面か「/」）を返す. */
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def devLogin(name: String, email: String, admin: Boolean, returnTo: String): Future[Json] =
    post[Json]("/auth/dev-login", Json.obj("name" -> name.asJson, "email" -> email.asJson, "admin" -> admin.asJson, "returnTo" -> returnTo.asJson))

  def mailboxes(): Future[List[MailboxDto]] = get[List[MailboxDto]]("/api/mailboxes")
  def messages(box: Option[UUID], q: String, offset: Int, limit: Int): Future[MessageListDto] =
    get[MessageListDto]("/api/messages" + query("mailboxId" -> box.map(_.toString), "q" -> Some(q), "offset" -> Some(offset.toString),
      "limit" -> Some(limit.toString)))
  def thread(id: UUID): Future[ThreadDto] = get[ThreadDto](s"/api/messages/$id/thread?markRead=true")
  def update(r: UpdateMessagesReq): Future[UpdatedDto] = post[UpdatedDto]("/api/messages/update", r.asJson)
  def contacts(q: String): Future[List[ContactDto]] = get[List[ContactDto]]("/api/contacts" + query("q" -> Some(q)))

}
