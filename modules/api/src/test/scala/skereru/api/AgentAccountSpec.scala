package skereru.api

import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import skereru.shared.*

import java.util.UUID

/** エージェント専用アカウント（管理者が作り、APIキーを発行する）の API の試験。TEST_DATABASE_URL がなければ飛ばす（ApiTestEnv）。 */
class AgentAccountSpec extends ApiTestEnv:

  withEnv("管理者がエージェント専用アカウントを作り、APIキーで MCP・REST を使える") { e =>
    val tag = UUID.randomUUID().toString.take(8)
    val domain = s"agent$tag.example"
    for
      adminC <- devLogin(e, s"admin-$tag", admin = true)
      userC  <- devLogin(e, s"user-$tag", admin = false)
      // 管理者でなければ 403
      r_st0 <- call(e, userC, Method.GET, "/api/admin/agents")
      (st0, _) = r_st0
      _ = assertEquals(st0, Status.Forbidden)
      r_st0b <- call(e, userC, Method.POST, "/api/admin/agents", Some(Json.obj("name" -> "x".asJson)))
      (st0b, _) = r_st0b
      _ = assertEquals(st0b, Status.Forbidden)
      // 名前の検査
      r_stBad <- call(e, adminC, Method.POST, "/api/admin/agents", Some(Json.obj("name" -> "  ".asJson)))
      (stBad, _) = r_stBad
      _ = assertEquals(stBad, Status.BadRequest)
      r_stBad2 <- call(e, adminC, Method.POST, "/api/admin/agents", Some(Json.obj("name" -> ("a" * 61).asJson)))
      (stBad2, _) = r_stBad2
      _ = assertEquals(stBad2, Status.BadRequest)
      // 送信用のアドレスつきで作る
      r_stD <- call(e, adminC, Method.POST, "/api/admin/domains", Some(Json.obj("name" -> domain.asJson, "autoProvision" -> false.asJson)))
      (stD, _) = r_stD
      _ = assertEquals(stD, Status.Created)
      r_st1 <- call(e, adminC, Method.POST, "/api/admin/agents", Some(Json.obj("name" -> s" skakapo-$tag ".asJson, "address" -> s"agent@$domain".asJson)))
      (st1, j1) = r_st1
      _ = assertEquals(st1, Status.Created, j1.noSpaces)
      agent = j1.as[AgentDto].fold(x => fail(x.getMessage), identity)
      _ = assertEquals(agent.name, s"skakapo-$tag")
      _ = assertEquals(agent.addresses, List(s"agent@$domain"))
      r__ <- call(e, adminC, Method.GET, "/api/admin/agents")
      (_, jl) = r__
      _ = assert(jl.as[List[AgentDto]].toOption.exists(_.exists(_.id == agent.id)))
      // ログインできない（sub は agent:、email はない）
      u <- e.users.find(agent.id).map(_.get)
      _ = assert(u.oidcSub.exists(_.startsWith("agent:")), u.oidcSub.toString)
      _ = assertEquals(u.email, None)
      byLogin <- e.users.login(u.oidcSub.get, "なりすまし", None, None, create = true)
      _ = assertEquals(byLogin, None)
      // admin スコープは付けられない
      r_stA <- call(e, adminC, Method.POST, s"/api/admin/agents/${agent.id}/keys", Some(Json.obj("name" -> "k".asJson, "scopes" -> List("read", "admin").asJson)))
      (stA, _) = r_stA
      _ = assertEquals(stA, Status.BadRequest)
      // ふつうのユーザーはエージェントとして扱えない
      me <- call(e, adminC, Method.GET, "/api/me").map(_._2.hcursor.get[UUID]("id").toOption.get)
      r_stN <- call(e, adminC, Method.POST, s"/api/admin/agents/$me/keys", Some(Json.obj("name" -> "k".asJson, "scopes" -> List("read").asJson)))
      (stN, _) = r_stN
      _ = assertEquals(stN, Status.NotFound)
      r_stN2 <- call(e, adminC, Method.PATCH, s"/api/admin/agents/$me", Some(Json.obj("disabled" -> true.asJson)))
      (stN2, _) = r_stN2
      _ = assertEquals(stN2, Status.NotFound)
      // 書き込みのキーを発行する
      r_stK <- call(e, adminC, Method.POST, s"/api/admin/agents/${agent.id}/keys",
                     Some(Json.obj("name" -> "skakapo".asJson, "scopes" -> List("read", "write", "send").asJson, "expiresDays" -> 30.asJson)))
      (stK, jk) = r_stK
      _ = assertEquals(stK, Status.Created, jk.noSpaces)
      created = jk.as[CreatedApiKeyDto].fold(x => fail(x.getMessage), identity)
      key = Who.Key(created.key)
      _ = assert(!jk.noSpaces.contains("hash"))
      // MCP の tools/list と REST の参照・書き込み
      r_stM <- call(e, key, Method.POST, "/mcp", Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> "tools/list".asJson)))
      (stM, jm) = r_stM
      _ = assertEquals(stM, Status.Ok)
      tools = jm.hcursor.downField("result").downField("tools").as[List[Json]].toOption.getOrElse(Nil).flatMap(_.hcursor.get[String]("name").toOption)
      _ = assert(tools.contains("send_message") && tools.contains("update_messages"), tools.toString)
      r_stR <- call(e, key, Method.GET, "/api/me")
      (stR, jr) = r_stR
      _ = assertEquals(stR, Status.Ok)
      _ = assertEquals(jr.hcursor.get[UUID]("id").toOption, Some(agent.id))
      r_stW <- call(e, key, Method.PATCH, "/api/me", Some(Json.obj("timezone" -> "UTC".asJson)))
      (stW, _) = r_stW
      _ = assertEquals(stW, Status.NoContent)
      // APIキーではエージェント専用アカウントを管理できない
      r_stKA <- call(e, key, Method.GET, "/api/admin/agents")
      (stKA, _) = r_stKA
      _ = assertEquals(stKA, Status.Forbidden)
      // 無効にするとキーが使えない。戻すと使える
      r_stDis <- call(e, adminC, Method.PATCH, s"/api/admin/agents/${agent.id}", Some(Json.obj("disabled" -> true.asJson)))
      (stDis, jd) = r_stDis
      _ = assertEquals(stDis, Status.Ok)
      _ = assertEquals(jd.hcursor.get[Boolean]("disabled").toOption, Some(true))
      r_stR2 <- call(e, key, Method.GET, "/api/me")
      (stR2, _) = r_stR2
      _ = assertEquals(stR2, Status.Unauthorized)
      _ <- call(e, adminC, Method.PATCH, s"/api/admin/agents/${agent.id}", Some(Json.obj("disabled" -> false.asJson, "name" -> "改名".asJson)))
      r_stR3 <- call(e, key, Method.GET, "/api/me")
      (stR3, _) = r_stR3
      _ = assertEquals(stR3, Status.Ok)
      // 失効するとキーが使えない
      r_stX <- call(e, adminC, Method.DELETE, s"/api/admin/agents/${agent.id}/keys/${created.info.id}")
      (stX, _) = r_stX
      _ = assertEquals(stX, Status.NoContent)
      r_stR4 <- call(e, key, Method.GET, "/api/me")
      (stR4, _) = r_stR4
      _ = assertEquals(stR4, Status.Unauthorized)
      r_stX2 <- call(e, adminC, Method.DELETE, s"/api/admin/agents/${agent.id}/keys/${created.info.id}")
      (stX2, _) = r_stX2
      _ = assertEquals(stX2, Status.NotFound)
      // 監査ログに残る
      logs <- call(e, adminC, Method.GET, "/api/admin/audit?action=agent.").map(_._2.as[List[AuditDto]].toOption.getOrElse(Nil))
      _ = assert(Set("agent.create", "agent.update", "agent.api_key.create", "agent.api_key.delete").subsetOf(logs.map(_.action).toSet), logs.map(_.action).toString)
    yield ()
  }
