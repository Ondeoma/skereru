package skereru.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import skereru.auth.SecretBox
import skereru.core.*
import skereru.db.*
import skereru.mail.{Delivery, MailConfig}
import skereru.shared.*

import java.time.{Instant, ZoneId}
import java.util.UUID

/** 会議の招待（メールの中の iCalendar）: 表示・返事（iMIP の REPLY）・skuaka のカレンダーへの登録。
  * skuaka の URL は管理者が設定したものだけ使う（利用者が URL を決められると、サーバーから任意の場所へつながせてしまう＝SSRF）。 */
final class InviteService(svc: MailService, users: UserStore, domains: DomainStore, mail: MailStore, delivery: Delivery, box: SecretBox,
    http: Client[IO], skuakaUrl: Option[String], mcfg: MailConfig):

  private val Skuaka = "skuaka"
  private val Statuses = Map("ACCEPTED" -> "参加します", "TENTATIVE" -> "仮承諾します", "DECLINED" -> "辞退します")

  private def zoneOf(u: User): ZoneId = Op.zone(u.timezone)

  /** 招待と、自分（招待された自分のアドレス）。 */
  private def loadInvite(s: AuthSession, id: UUID): Res[(MsgMeta, Invite, Option[IcalPerson])] =
    svc.load(s.user.id, id).flatMap {
      case Left(f) => IO.pure(Left(f))
      case Right((m, raw)) =>
        Ical.find(raw, Mime.parse(raw), zoneOf(s.user)) match
          case None => IO.pure(Left(Fail.NotFound("招待")))
          case Some(inv) =>
            domains.sendable(s.user.id).map { mine =>
              val me = inv.attendees.find(a => mine.contains(a.address))
              Right((m, inv, me.orElse(mine.headOption.filter(_ => inv.attendees.isEmpty).map(a => IcalPerson(a, None, None)))))
            }
    }

  private def ts(t: IcalTime, zone: ZoneId): Timestamp = t match
    case IcalTime.Day(d) => d.atStartOfDay(zone).toInstant
    case IcalTime.At(i)  => i

  def invite(s: AuthSession, id: UUID): Res[InviteDto] =
    loadInvite(s, id).flatMap {
      case Left(f) => IO.pure(Left(f))
      case Right((_, inv, me)) =>
        connected(s.user.id).map { cal =>
          val z = zoneOf(s.user)
          Right(InviteDto(inv.method, inv.summary, inv.description, inv.location, inv.allDay, ts(inv.start, z), ts(inv.end, z),
            inv.organizer.map(o => AddrDto(o.name, o.address)), inv.attendees.map(a => InviteAttendeeDto(a.address, a.name, a.partstat)),
            me.map(_.address), me.flatMap(_.partstat), inv.recurring,
            canReply = inv.method == "REQUEST" && me.isDefined && inv.organizer.exists(o => !me.exists(_.address == o.address)),
            canAddToCalendar = cal && (inv.method == "REQUEST" || inv.method == "PUBLISH")))
        }
    }

  /** 主催者に返事を送る（iMIP の REPLY）。 */
  def reply(s: AuthSession, id: UUID, partstat0: String, ip: String): Res[Unit] =
    val partstat = partstat0.trim.toUpperCase
    if !Statuses.contains(partstat) then IO.pure(Left(Fail.Invalid("返事は ACCEPTED・TENTATIVE・DECLINED のどれかです")))
    else loadInvite(s, id).flatMap {
      case Left(f) => IO.pure(Left(f))
      case Right((m, inv, me)) =>
        (me, inv.organizer) match
          case (Some(i), Some(org)) if inv.method == "REQUEST" =>
            IO.realTimeInstant.flatMap { now =>
              val name = Some(s.user.name).filter(_.nonEmpty)
              val ics = Ical.reply(inv, i.address, name, partstat, now)
              val label = Statuses(partstat)
              val o = Outgoing(MailAddress(name, Address.localOf(i.address), Address.domainOf(i.address)), List(MailAddress(org.name, Address.localOf(org.address), Address.domainOf(org.address))),
                Nil, s"${label.stripSuffix("します")}: ${inv.summary}", s"${s.user.name} は「${inv.summary}」に$label。", None, Nil, None, Nil,
                calendar = Some("REPLY" -> ics))
              val raw = Compose.build(o, Compose.newMessageId(Address.domainOf(i.address)), now, zoneOf(s.user))
              delivery.submit(s.user, i.address, List(org.address), raw, "HTTPS", Some(ip)).flatMap {
                case Left(e)  => IO.pure(Left(Fail.Forbidden(e)))
                case Right(_) => mail.setFlags(s.user.id, List(m.id), List(Flag.Answered), Nil).as(Right(()))
              }
            }
          case _ => IO.pure(Left(Fail.Invalid("この招待には返事できません（宛先に自分のアドレスがありません）")))
    }

  // ---------------------------------------------------------------- skuaka

  def connected(userId: UUID): IO[Boolean] = if skuakaUrl.isEmpty then IO.pure(false) else users.integration(userId, Skuaka).map(_.isDefined)

  def integrations(userId: UUID): IO[IntegrationsDto] = connected(userId).map(c => IntegrationsDto(skuakaUrl, c))

  private def call(token: String, req: Request[IO]): IO[Either[String, Json]] =
    http.run(req.putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, token)))).use { r =>
      if r.status.isSuccess then r.as[Json].map(Right(_))
      else if r.status == Status.Unauthorized then IO.pure(Left("skuaka のトークンが無効です。設定でつなぎ直してください"))
      else r.as[Json].attempt.map(j => Left(j.toOption.flatMap(_.hcursor.get[String]("error").toOption).getOrElse(s"skuaka がエラーを返しました（${r.status.code}）")))
    }.handleError(e => Left(s"skuaka につながりません: ${Option(e.getMessage).getOrElse(e.toString).take(200)}"))

  private def base: Uri = Uri.unsafeFromString(skuakaUrl.get.stripSuffix("/"))

  /** 書き込めるカレンダー（いちばん上のもの＝主のカレンダーを先に）。 */
  private def calendar(token: String): IO[Either[String, String]] =
    call(token, Request[IO](Method.GET, base / "api" / "calendars")).map(_.flatMap { j =>
      val cals = j.asArray.getOrElse(Vector.empty).flatMap { c =>
        val h = c.hcursor
        for id <- h.get[String]("id").toOption; role <- h.get[String]("role").toOption
        yield (id, role, h.get[Boolean]("primary").getOrElse(false))
      }.filter((_, role, _) => role == "owner" || role == "edit")
      cals.sortBy(c => if c._3 then 0 else 1).headOption.map(_._1).toRight("skuaka に書き込めるカレンダーがありません")
    })

  /** トークンを確かめて保存する（暗号化して置く）。 */
  def connect(userId: UUID, token: String): Res[Unit] =
    if skuakaUrl.isEmpty then IO.pure(Left(Fail.Invalid("このサーバーでは skuaka と連携できません（管理者が SKUAKA_URL を設定していません）")))
    else if token.trim.isEmpty || token.length > 500 then IO.pure(Left(Fail.Invalid("トークンを入力してください")))
    else calendar(token.trim).flatMap {
      case Left(e)  => IO.pure(Left(Fail.Invalid(e)))
      case Right(_) => users.setIntegration(userId, Skuaka, box.encrypt(token.trim)).as(Right(()))
    }

  def disconnect(userId: UUID): IO[Unit] = users.deleteIntegration(userId, Skuaka).void

  /** 招待を skuaka のカレンダーに予定として足す。 */
  def addToCalendar(s: AuthSession, id: UUID): Res[CalendarAddedDto] =
    if skuakaUrl.isEmpty then IO.pure(Left(Fail.Invalid("skuaka と連携していません")))
    else users.integration(s.user.id, Skuaka).map(_.flatMap(box.decrypt)).flatMap {
      case None => IO.pure(Left(Fail.Invalid("設定の「連携」で skuaka につないでください")))
      case Some(token) =>
        loadInvite(s, id).flatMap {
          case Left(f) => IO.pure(Left(f))
          case Right((_, inv, _)) =>
            calendar(token).flatMap {
              case Left(e) => IO.pure(Left(Fail.Invalid(e)))
              case Right(calId) =>
                // skuaka の終日の予定は終わりの日を含む（iCalendar の DTEND は含まない）
                val (start, end) = (inv.start, inv.end) match
                  case (IcalTime.Day(a), IcalTime.Day(b)) => (a.toString, (if b.isAfter(a) then b.minusDays(1) else a).toString)
                  case (a, b)                             => (ts(a, zoneOf(s.user)).toString, ts(b, zoneOf(s.user)).toString)
                val desc = (inv.description + (if inv.recurring then "\n\n（繰り返しの予定です。最初の回だけを追加しました）" else "")).trim
                val body = Json.obj("calendarId" -> calId.asJson, "title" -> (if inv.summary.isEmpty then "（無題の予定）" else inv.summary).asJson,
                  "description" -> desc.asJson, "location" -> inv.location.asJson, "allDay" -> inv.allDay.asJson, "start" -> start.asJson,
                  "end" -> end.asJson, "timezone" -> s.user.timezone.asJson, "color" -> Json.Null, "recurrence" -> Json.Null,
                  "attendees" -> Json.arr(), "reminders" -> Json.arr(10.asJson), "version" -> Json.Null)
                call(token, Request[IO](Method.POST, base / "api" / "events").withEntity(body)).map(_.left.map(Fail.Invalid(_)).map(_ => CalendarAddedDto(skuakaUrl.get)))
            }
        }
    }
