package skereru.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import skereru.core.Mime
import skereru.shared.*

import java.nio.charset.StandardCharsets
import java.util.UUID

/** 受信の通知先（受信 Webhook・署名付きの Webhook）と、振り分けの規則の「通知先に知らせる」. */
class NotifySpec extends ApiTestEnv {

  private def str(j: Json, k: String): String = j.hcursor.get[String](k).getOrElse(fail(s"$k がありません: ${j.noSpaces}"))

  private def target(e: Env, who: Who, name: String, kind: String, url: String, allInbox: Boolean = true): IO[Json] =
    call(
      e,
      who,
      Method.POST,
      "/api/me/notify-targets",
      Some(NotifyTargetReq(name, kind, None, None, Some(url), allInbox, true).asJson)
    ).map { (st, j) => assertEquals(st, Status.Created, j.noSpaces); j }

  private def mail(subject: String): Array[Byte] =
    (s"From: =?UTF-8?B?${java.util.Base64.getEncoder.encodeToString("山田 太郎".getBytes(StandardCharsets.UTF_8))}?= <taro@example.com>\r\n" +
      s"To: me@test.local\r\nSubject: $subject\r\nMessage-ID: <${UUID.randomUUID()}@example.com>\r\n\r\n秘密の本文\r\n")
      .getBytes(StandardCharsets.UTF_8)

  /** 送信待ちを送り切って、送った要求（本文）を取り出す. */
  private def flush(e: Env): IO[List[(Request[IO], String)]] =
    e.notifyStore.processDeliveries(50)(e.notifier.deliver) *> IO {
      val l = Iterator.continually(e.hooks.poll()).takeWhile(_ != null).toList
      l
    }

  test("stui への文: 見出し（件名）はエスケープせず stui に任せ、本文（差出人）はエスケープする") {
    val p = NotifyService.mailPayload(
      UUID.randomUUID(),
      Mime.parse(mail("[至急] *重要*")),
      "INBOX",
      java.time.Instant.EPOCH,
      "http://x"
    )
    val m = NotifyService.stuiMessage(p)
    assertEquals(m.title, "📩 [至急] *重要*")
    assert(m.text.contains("山田 太郎 \\<taro@example.com\\>"), m.text)
  }

  withEnv("Webhook の通知先: 署名の鍵・試しの送信・受信トレイの新着・規則で選んだときだけ・本文は送らない") { e =>
    for {
      who <- devLogin(e, "notify-a", admin = false)
      me <- call(e, who, Method.GET, "/api/me").map(_._2)
      uid = UUID.fromString(str(me, "id"))
      _ <- IO(e.hooks.clear())
      // 前の試験で作った通知先を消す（上限に当たらないように）
      old <- e.notifyStore.targets(uid)
      _ <- old.traverse_(t => e.notifyStore.delete(uid, t.id))
      // http(s) 以外の URL は断る
      bad <- call(
        e,
        who,
        Method.POST,
        "/api/me/notify-targets",
        Some(NotifyTargetReq("x", NotifyKind.Webhook, None, None, Some("ftp://hooks.example/x"), true, true).asJson)
      )
      _ = assertEquals(bad._1, Status.BadRequest)
      // stui の連携が設定されていないサーバーでは、stui の通知先は作れない
      noStui <- call(
        e,
        who,
        Method.POST,
        "/api/me/notify-targets",
        Some(NotifyTargetReq("dm", NotifyKind.Stui, None, None, None, true, true).asJson)
      )
      _ = assertEquals(noStui._1, Status.BadRequest)
      wh <- target(e, who, "webhook", NotifyKind.Webhook, "https://hooks.example/in")
      secret = str(wh, "secret")
      _ = assert(secret.length >= 32, wh.noSpaces)
      chat <- target(e, who, "chat", NotifyKind.ChatHook, "https://chat.example/hooks/abc", allInbox = false)
      chatId = str(chat, "id")

      // 試しの送信: その場で送る. 署名は本文の HMAC-SHA256
      t <- call(e, who, Method.POST, s"/api/me/notify-targets/${str(wh, "id")}/test")
      _ = assertEquals(t._1, Status.NoContent)
      sent <- IO(e.hooks.poll())
      _ = assertEquals(sent._1.uri.renderString, "https://hooks.example/in")
      _ = assertEquals(
        sent._1.headers.get(org.typelevel.ci.CIString("X-Skereru-Signature")).map(_.head.value),
        Some(sbirds.links.Webhook.signature(secret, sent._2))
      )
      _ =
        assertEquals(sent._1.headers.get(org.typelevel.ci.CIString("X-Skereru-Event")).map(_.head.value), Some("test"))

      // 受信トレイの新着: allInbox の Webhook にだけ送る（chat は規則で選んだときだけ）
      id1 = UUID.randomUUID()
      _ <- e.notifier.mailArrived(uid, id1, Mime.parse(mail("会議の件")), "INBOX", inbox = true, Nil)
      l1 <- flush(e)
      _ = assertEquals(l1.map(_._1.uri.renderString), List("https://hooks.example/in"))
      p1 = io.circe.parser.parse(l1.head._2).toOption.get
      _ = assertEquals(p1.hcursor.get[String]("event").toOption, Some("mail.received"))
      _ = assertEquals(p1.hcursor.get[String]("subject").toOption, Some("会議の件"))
      _ = assertEquals(p1.hcursor.downField("from").get[String]("name").toOption, Some("山田 太郎"))
      _ = assertEquals(p1.hcursor.downField("from").get[String]("address").toOption, Some("taro@example.com"))
      _ = assertEquals(p1.hcursor.get[String]("url").toOption, Some(s"http://localhost/m/$id1"))
      _ = assert(!l1.head._2.contains("秘密の本文"), "本文は送らない")

      // 規則で選んだ通知先（受信トレイ以外のフォルダ）: chat にだけ送る. 件名の Markdown の記号は無害にする
      _ <-
        e.notifier.mailArrived(uid, UUID.randomUUID(), Mime.parse(mail("[至急] *重要*")), "仕事", inbox = false, List(chatId))
      l2 <- flush(e)
      _ = assertEquals(l2.map(_._1.uri.renderString), List("https://chat.example/hooks/abc"))
      text = io.circe.parser.parse(l2.head._2).toOption.get.hcursor.get[String]("text").getOrElse("")
      _ = assert(text.contains("\\[至急\\] \\*重要\\*"), text)
      _ = assert(text.contains("フォルダ: 仕事"), text)
      _ = assert(text.contains("山田 太郎 \\<taro@example.com\\>"), text)

      // 無効にした通知先には送らない
      _ <- call(
        e,
        who,
        Method.PUT,
        s"/api/me/notify-targets/${str(wh, "id")}",
        Some(NotifyTargetReq(
          "webhook",
          NotifyKind.Webhook,
          None,
          None,
          Some("https://hooks.example/in"),
          true,
          false
        ).asJson)
      )
      _ <- e.notifier.mailArrived(uid, UUID.randomUUID(), Mime.parse(mail("x")), "INBOX", inbox = true, Nil)
      l3 <- flush(e)
      _ = assertEquals(l3, Nil)
    } yield ()
  }

  withEnv("規則の「通知先に知らせる」: 自分の通知先だけ選べる. ほかの人の通知先の ID では送らない. 削除") { e =>
    for {
      a <- devLogin(e, "notify-b", admin = false)
      b <- devLogin(e, "notify-c", admin = false)
      bMe <- call(e, b, Method.GET, "/api/me").map(_._2)
      bId = UUID.fromString(str(bMe, "id"))
      aTarget <- target(e, a, "a の通知先", NotifyKind.ChatHook, "https://chat.example/hooks/a", allInbox = false)
      aTargetId = str(aTarget, "id")
      rule = (arg: String) =>
        FilterRuleReq(
          "通知",
          true,
          true,
          List(RuleConditionDto(RuleField.Subject, RuleOp.Contains, "至急", None)),
          List(RuleActionDto(RuleAction.Notify, Some(arg)))
        ).asJson
      ok <- call(e, a, Method.POST, "/api/filters", Some(rule(aTargetId)))
      _ = assertEquals(ok._1, Status.Created, ok._2.noSpaces)
      ng <- call(e, b, Method.POST, "/api/filters", Some(rule(aTargetId)))
      _ = assertEquals(ng._1, Status.BadRequest, ng._2.noSpaces)
      // 配送の側でも、ほかの人の通知先は拾わない
      _ <- IO(e.hooks.clear())
      _ <-
        e.notifier.mailArrived(bId, UUID.randomUUID(), Mime.parse(mail("至急")), "INBOX", inbox = true, List(aTargetId))
      l <- flush(e)
      _ = assertEquals(l, Nil)
      // ほかの人の通知先は消せない・見えない
      del <- call(e, b, Method.DELETE, s"/api/me/notify-targets/$aTargetId")
      _ = assertEquals(del._1, Status.NotFound)
      list <- call(e, b, Method.GET, "/api/me/notify-targets")
      _ = assertEquals(list._2.asArray.map(_.size), Some(0))
      del2 <- call(e, a, Method.DELETE, s"/api/me/notify-targets/$aTargetId")
      _ = assertEquals(del2._1, Status.NoContent)
    } yield ()
  }
}
