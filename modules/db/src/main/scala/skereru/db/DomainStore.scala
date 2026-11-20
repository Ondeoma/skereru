package skereru.db

import sbirds.db.*
import skereru.core.ErrorCode

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skereru.shared.*

import java.util.UUID

/** ドメインと、配送先のアドレス（主アドレス・別名・転送）。 */
final class DomainStore(xa: Transactor[IO]) extends TxStore(xa) {

  private val dcols =
    fr"id, name, enabled, auto_provision, catch_all, dkim_selector, dkim_private, dkim_public, created_at"
  private val acols = fr"a.id, a.address, a.domain_id, a.user_id, u.name, a.forward_to, a.is_primary, a.can_send"

  def domains: IO[List[Domain]] = tx((fr"select" ++ dcols ++ fr"from domain order by name").query[Domain].to[List])

  def domainDtos: IO[List[DomainDto]] = tx(
    sql"""select d.id, d.name, d.enabled, d.auto_provision, d.catch_all, d.dkim_selector, (select count(*)::int from address a where a.domain_id = d.id), d.created_at
          from domain d order by d.name""".query[DomainDto].to[List]
  )

  def domain(id: UUID): IO[Option[Domain]] =
    tx((fr"select" ++ dcols ++ fr"from domain where id = $id").query[Domain].option)

  /** 有効なドメイン（名前で。大文字小文字は区別しない）。 */
  def domainByName(name: String): IO[Option[Domain]] = tx(domainByNameC(name))
  def domainByNameC(name: String): ConnectionIO[Option[Domain]] =
    (fr"select" ++ dcols ++ fr"from domain where name = ${name.toLowerCase.stripSuffix(".")} and enabled").query[
      Domain
    ].option

  def isLocalDomain(name: String): IO[Boolean] = domainByName(name).map(_.isDefined)

  def createDomain(name: String,
                   autoProvision: Boolean,
                   selector: String,
                   priv: String,
                   pub: String): IO[Either[AppError, UUID]] = tx(
    sql"select 1 from domain where name = $name".query[Int].option.flatMap {
      case Some(_) => fail(AppError(ErrorCode.DomainAlreadyExists, s"ドメイン $name は登録済みです"))
      case None =>
        val id = UUID.randomUUID()
        sql"""insert into domain (id, name, auto_provision, dkim_selector, dkim_private, dkim_public) values ($id, $name, $autoProvision, $selector, $priv, $pub)"""
          .update.run.as(Right(id))
    }
  )

  def updateDomain(id: UUID,
                   enabled: Option[Boolean],
                   autoProvision: Option[Boolean],
                   catchAll: Option[Option[UUID]]): IO[Int] = tx {
    val sets = List(
      enabled.map(e => fr"enabled = $e"),
      autoProvision.map(a => fr"auto_provision = $a"),
      catchAll.map(c => fr"catch_all = $c")
    ).flatten
    if sets.isEmpty then FC.pure(0) else (fr"update domain" ++ Sql.set(sets) ++ fr"where id = $id").update.run
  }

  def rotateDkim(id: UUID, selector: String, priv: String, pub: String): IO[Int] = tx(
    sql"update domain set dkim_selector = $selector, dkim_private = $priv, dkim_public = $pub where id = $id".update.run
  )

  def deleteDomain(id: UUID): IO[Int] = tx(sql"delete from domain where id = $id".update.run)

  // ---------------------------------------------------------------- アドレス

  def addresses(domainId: Option[UUID], userId: Option[UUID]): IO[List[AddressRow]] = tx(addressesC(domainId, userId))
  def addressesC(domainId: Option[UUID], userId: Option[UUID]): ConnectionIO[List[AddressRow]] =
    (fr"select" ++ acols ++ fr"from address a left join app_user u on u.id = a.user_id" ++
      Sql.whereAnd(List(domainId.map(d => fr"a.domain_id = $d"), userId.map(u => fr"a.user_id = $u")).flatten) ++
      fr"order by a.address").query[AddressRow].to[List]

  def address(addr: String): IO[Option[AddressRow]] = tx(addressC(addr))
  def addressC(addr: String): ConnectionIO[Option[AddressRow]] =
    (fr"select" ++ acols ++
      fr"from address a left join app_user u on u.id = a.user_id join domain d on d.id = a.domain_id where a.address = ${addr.toLowerCase} and d.enabled")
      .query[AddressRow].option

  def addressById(id: UUID): IO[Option[AddressRow]] = tx((fr"select" ++ acols ++
    fr"from address a left join app_user u on u.id = a.user_id where a.id = $id").query[AddressRow].option)

  def createAddress(address: String,
                    domainId: UUID,
                    userId: Option[UUID],
                    forwardTo: List[String],
                    isPrimary: Boolean,
                    canSend: Boolean)
    : ConnectionIO[Either[AppError, UUID]] =
    sql"select 1 from address where address = $address".query[Int].option.flatMap {
      case Some(_) => fail(AppError(ErrorCode.AddressInUse, s"$address は使われています"))
      case None =>
        val id = UUID.randomUUID()
        val primary = isPrimary && userId.isDefined
        (if primary then sql"update address set is_primary = false where user_id = $userId".update.run
         else FC.pure(0)) *>
          sql"""insert into address (id, address, domain_id, user_id, forward_to, is_primary, can_send) values ($id, $address, $domainId, $userId, $forwardTo, $primary, $canSend)"""
            .update.run.as(Right(id))
    }

  def createAddressIO(address: String,
                      domainId: UUID,
                      userId: Option[UUID],
                      forwardTo: List[String],
                      isPrimary: Boolean,
                      canSend: Boolean)
    : IO[Either[AppError, UUID]] = tx(createAddress(address, domainId, userId, forwardTo, isPrimary, canSend))

  def updateAddress(id: UUID, userId: Option[UUID], forwardTo: List[String], canSend: Boolean): IO[Int] = tx(
    sql"update address set user_id = $userId, forward_to = $forwardTo, can_send = $canSend, is_primary = is_primary and $userId is not null where id = $id".update.run
  )

  def setPrimary(userId: UUID, id: UUID): IO[Int] =
    tx(sql"update address set is_primary = false where user_id = $userId".update.run *>
      sql"update address set is_primary = true where id = $id and user_id = $userId".update.run)

  def deleteAddress(id: UUID): IO[Int] = tx(sql"delete from address where id = $id".update.run)

  /** ユーザーが From に使えるアドレス（自分のアドレスで can_send のもの）。 */
  def sendable(userId: UUID): IO[List[String]] = tx(
    sql"""select a.address from address a join domain d on d.id = a.domain_id where a.user_id = $userId and a.can_send and d.enabled
          order by a.is_primary desc, a.address""".query[String].to[List]
  )

  def primaryAddress(userId: UUID): IO[Option[String]] =
    tx(sql"select address from address where user_id = $userId order by is_primary desc, created_at limit 1".query[
      String
    ].option)
}
