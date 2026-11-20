package skereru.web

import io.circe.*
import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/**
 * バックエンドの REST API（共通部分は sbirds-lib の ApiBase）. 401 はログイン画面へ、エラーは {"error": ...} の文言にする.
 * 要求には X-Skereru ヘッダー（Main で Fetch.extraHeaders に設定）を付ける（サーバーはこれがない Cookie の更新を拒否する＝CSRF 対策）.
 */
object Api extends sbirds.web.ApiBase {

  /** バイナリの本文を送る（application/octet-stream）. */
  def postBytes[A: Decoder](url: String, bytes: js.typedarray.Uint8Array): Future[A] =
    postRaw[A](url, bytes.asInstanceOf[dom.BodyInit], Some("application/octet-stream"))

  /** 画像などをそのまま（Content-Type はファイルの種類）PUT で送る. */
  def putBlob(url: String, data: dom.Blob): Future[Unit] = putRaw[Json](url, data, Some(data.`type`)).map(_ => ())

  /** ファイルを multipart/form-data（file）で送る. */
  def upload(file: dom.File): Future[UploadDto] = postFile[UploadDto]("/api/uploads", file, file.name)

  // ---------------------------------------------------------------- よく使うもの

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")

  /** ログアウトして、次に開く URL（IdP のログアウト画面か「/」）を返す. */
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def devLogin(name: String, email: String, admin: Boolean, returnTo: String): Future[Json] =
    post[Json](
      "/auth/dev-login",
      Json.obj("name" -> name.asJson, "email" -> email.asJson, "admin" -> admin.asJson, "returnTo" -> returnTo.asJson)
    )

  def mailboxes(): Future[List[MailboxDto]] = get[List[MailboxDto]]("/api/mailboxes")
  def messages(box: Option[UUID], q: String, offset: Int, limit: Int): Future[MessageListDto] =
    get[MessageListDto]("/api/messages" + query(
      "mailboxId" -> box.map(_.toString),
      "q" -> Some(q),
      "offset" -> Some(offset.toString),
      "limit" -> Some(limit.toString)
    ))
  def thread(id: UUID): Future[ThreadDto] = get[ThreadDto](s"/api/messages/$id/thread?markRead=true")
  def update(r: UpdateMessagesReq): Future[UpdatedDto] = post[UpdatedDto]("/api/messages/update", r.asJson)
  def contacts(q: String): Future[List[ContactDto]] = get[List[ContactDto]]("/api/contacts" + query("q" -> Some(q)))

}
