package skereru.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import skereru.shared.*

import java.util.UUID

/** skawau（ファイル共有）との連携: ファイルを添付する・リンクを入れる・添付を保存する（手で・規則の保存待ちで）. */
class SkawauSpec extends ApiTestEnv {

  private def str(j: Json, k: String): String = j.hcursor.get[String](k).getOrElse(fail(s"$k がありません: ${j.noSpaces}"))

  withEnv("連携していなければ断る. 連携したら選ぶ・添付する・リンクを入れる・保存する. トークンが失効したら連携を外す") { e =>
    val f = e.skawau
    for {
      who <- devLogin(e, "skawau-a", admin = true)
      me <- call(e, who, Method.GET, "/api/me").map(_._2)
      uid = UUID.fromString(str(me, "id"))
      // 送れるアドレス（下書きの差出人）
      dom = s"skawau${UUID.randomUUID().toString.take(8)}.example"
      _ <- call(
        e,
        who,
        Method.POST,
        "/api/admin/domains",
        Some(Json.obj("name" -> dom.asJson, "autoProvision" -> false.asJson))
      )
      addr = s"a@$dom"
      ad <- call(
        e,
        who,
        Method.POST,
        "/api/admin/addresses",
        Some(Json.obj(
          "address" -> addr.asJson,
          "userId" -> uid.asJson,
          "forwardTo" -> Json.arr(),
          "canSend" -> true.asJson
        ))
      )
      _ = assert(ad._1.isSuccess, ad._2.noSpaces)

      // 連携していない（前の試験の残りを消す）
      _ <- e.tokens.deleteToken(uid, "skawau")
      st0 <- call(e, who, Method.GET, "/api/integrations/skawau").map(_._2)
      _ = assertEquals(st0.hcursor.get[Boolean]("connected").toOption, Some(false))
      ng <- call(e, who, Method.POST, "/api/integrations/skawau/attach", Some(SkawauAttachReq(List(f.file)).asJson))
      _ = assertEquals(ng._1, Status.BadRequest)
      _ = assertEquals(str(ng._2, "code"), "SKERERU-4302")

      // 連携した（トークンを置く）
      _ <- e.tokens.saveToken(uid, "skawau", e.box.encrypt("skawau-token"))
      st1 <- call(e, who, Method.GET, "/api/integrations/skawau").map(_._2)
      _ = assertEquals(st1.hcursor.get[Boolean]("connected").toOption, Some(true))
      root <- call(e, who, Method.GET, "/api/integrations/skawau/browse").map(_._2)
      _ = assertEquals(str(root, "name"), "すべてのファイル")
      _ = assertEquals(
        root.hcursor.downField("items").as[List[SkawauItemDto]].map(_.map(_.name)).toOption,
        Some(List("大きい.mov", "見積書.pdf"))
      )

      // 添付にする（skereru のアップロードになる）. 大きすぎるものは断る
      at <- call(e, who, Method.POST, "/api/integrations/skawau/attach", Some(SkawauAttachReq(List(f.file)).asJson))
      _ = assertEquals(at._1, Status.Ok, at._2.noSpaces)
      up = at._2.as[List[UploadDto]].toOption.flatMap(_.headOption).getOrElse(fail(at._2.noSpaces))
      _ = assertEquals((up.filename, up.size), ("見積書.pdf", f.content.length.toLong))
      big <- call(e, who, Method.POST, "/api/integrations/skawau/attach", Some(SkawauAttachReq(List(f.big)).asJson))
      _ = assertEquals(big._1.code, 413)

      // リンク
      ln <- call(e, who, Method.POST, "/api/integrations/skawau/links", Some(SkawauAttachReq(List(f.file)).asJson))
      _ = assertEquals(
        ln._2.as[List[SkawauLinkDto]].toOption.map(_.map(_.url)),
        Some(List(s"${FakeSkawau.Url}/view/${f.file}"))
      )

      // 添付付きの下書きを作り、その添付を skawau の「メールの添付」に保存する
      _ <- call(e, who, Method.GET, "/api/mailboxes") // 既定のメールボックス（下書きなど）を作る
      dr <- call(
        e,
        who,
        Method.POST,
        "/api/drafts",
        Some(ComposeReq(addr, Nil, Nil, Nil, "見積", "本文", None, List(up.id), None, None, None).asJson)
      )
      _ = assertEquals(dr._1, Status.Created, dr._2.noSpaces)
      draft = UUID.fromString(str(dr._2, "draftId"))
      sv <- call(e, who, Method.POST, "/api/integrations/skawau/save", Some(SkawauSaveReq(draft, Nil, None).asJson))
      _ = assertEquals(sv._1, Status.Ok, sv._2.noSpaces)
      _ = assertEquals(str(sv._2, "folderUrl"), s"${FakeSkawau.Url}/files/${f.mailFolder}")
      u1 <- IO(f.uploads.poll())
      _ = assertEquals((u1._1, u1._2, u1._3), (f.root, Some("メールの添付"), "見積書.pdf"))
      _ = assertEquals(u1._4.toList, f.content.toList)

      // 規則の「添付を skawau に保存」: 保存待ちに積んで、裏の処理で保存する
      _ <- e.skawauSvc.enqueue(uid, draft)
      n <- e.skawauSvc.processQueue
      _ = assert(n >= 1)
      u2 <- IO(f.uploads.poll())
      _ = assertEquals(Option(u2).map(_._3), Some("見積書.pdf"))

      // トークンが失効していたら（401）、保存していたトークンを消して「連携していない」にする
      _ <- e.tokens.saveToken(uid, "skawau", e.box.encrypt("revoked"))
      ex <- call(e, who, Method.GET, "/api/integrations/skawau/browse")
      _ = assertEquals(ex._1, Status.BadRequest)
      st2 <- call(e, who, Method.GET, "/api/integrations/skawau").map(_._2)
      _ = assertEquals(st2.hcursor.get[Boolean]("connected").toOption, Some(false))
    } yield ()
  }
}
