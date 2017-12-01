// package org.hablapps.stateless
// package test
//
// import scalaz._, Scalaz._
// import org.hablapps.puretest._
// import doobie.imports._
//
// import ZipCodeSpecDoobie._
//
// class ZipCodeSpecDoobie extends ZipCodeSpec.ScalaTest[ConnectionIO](
//   Sys, ZipCodeDoobie.doobieView, tester
// )
//
// object ZipCodeSpecDoobie{
//
//   implicit val HError = new HandleError[ConnectionIO,Throwable]{
//     def handleError[A](fa: ConnectionIO[A])(
//         f: Throwable => ConnectionIO[A]): ConnectionIO[A] =
//       Catchable[ConnectionIO].attempt(fa) flatMap {
//         _.fold(f,_.pure[ConnectionIO])
//       }
//   }
//
//   implicit val RError = new RaiseError[ConnectionIO,PuretestError[Throwable]]{
//     def raiseError[A](e: PuretestError[Throwable]): ConnectionIO[A] =
//       doobie.free.connection.fail(new RuntimeException(e.toString))
//   }
//
//   val xa = DriverManagerTransactor[IOLite](
//     "org.postgresql.Driver", "jdbc:postgresql:testing", "postgres", "postgres"
//   )
//
//   val Sys = ZipCodeDoobie.doobieSystem
//
//   val tester = new Tester[ConnectionIO,PuretestError[Throwable]]{
//     def apply[X](t: ConnectionIO[X]): Either[PuretestError[Throwable],X] =
//       (Sys.destroy >> Sys.create >> t)
//         .transact[IOLite](xa)
//         .attempt
//         .map(_.leftMap(ApplicationError(_)))
//         .unsafePerformIO
//         .toEither
//   }
// }
