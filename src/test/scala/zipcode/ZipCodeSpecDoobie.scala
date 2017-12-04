package org.hablapps.stateless
package test

import scalaz._, Scalaz._
import org.hablapps.puretest._
import doobie.imports._

import ZipCodeSpecDoobie._

class ZipCodeSpecDoobie
    extends scalatestImpl.FunSpec[ConnectionIO, Throwable]
    with ZipCodeSpec[ConnectionIO] {

  implicit val M = ZipCodeSpecDoobie.M
  implicit val RE = ZipCodeSpecDoobie.RError
  implicit val HE = ZipCodeSpecDoobie.HError

  val Sys = ZipCodeSpecDoobie.Sys
  val Ser = ZipCodeDoobie.doobieView
  val Tester = ZipCodeSpecDoobie.tester

  val Alg = new DepartmentAlg[Int] {
    type F[X] = ConnectionIO[X]
    type Pr = Int

    val Person = new PersonAlg[Int] {
      type F[X] = ConnectionIO[X]

      def init(per: SPerson) =
        for {
          oaid <- per.address.fold(
            Option.empty[Int].point[ConnectionIO])(
            add => sql"INSERT INTO Address (city, zip) VALUES (${add.city}, ${add.zip});"
              .update
              .withUniqueGeneratedKeys[Int]("aid")
              .map(Some(_)))
          pid <- sql"INSERT INTO Person (name, add) VALUES (${per.name}, $oaid);"
            .update
            .withUniqueGeneratedKeys[Int]("pid")
        } yield pid
    }

    def init(dep: SDepartment) = ???
  }

  val Lift = λ[Alg.Person.F ~> ConnectionIO](x => x)
}

object ZipCodeSpecDoobie{

  val M = Monad[ConnectionIO]

  val HError = new HandleError[ConnectionIO,Throwable]{
    def handleError[A](fa: ConnectionIO[A])(
        f: Throwable => ConnectionIO[A]): ConnectionIO[A] =
      Catchable[ConnectionIO].attempt(fa) flatMap {
        _.fold(f,_.pure[ConnectionIO])
      }
  }

  val RError = new RaiseError[ConnectionIO,PuretestError[Throwable]]{
    def raiseError[A](e: PuretestError[Throwable]): ConnectionIO[A] =
      doobie.free.connection.fail(new RuntimeException(e.toString))
  }

  val xa = DriverManagerTransactor[IOLite](
    "org.postgresql.Driver", "jdbc:postgresql:testing", "postgres", "postgres"
  )

  val Sys = ZipCodeDoobie.doobieSystem

  val tester = new Tester[ConnectionIO,PuretestError[Throwable]]{
    def apply[X](t: ConnectionIO[X]): Either[PuretestError[Throwable],X] =
      (Sys.destroy >> Sys.create >> t)
        .transact[IOLite](xa)
        .attempt
        .map(_.leftMap(ApplicationError(_)))
        .unsafePerformIO
        .toEither
  }
}
