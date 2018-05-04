// package org.hablapps.stateless
// package test
// package geofence

// import scalaz.Monad

// import org.scalatest._
// import puretest._, scalatestImpl._, ProgramStateMatchers.Syntax._

// trait MyTest[S, P[_]] extends GeofenceSpec[P] { self: FunSpec with Matchers =>

//   val sys: System.WithP[S, P]

//   implicit val Monad2: Monad[sys.geofence.P]

//   implicit val Monad3: Monad[sys.timer.P]

//   implicit val Tester: StateTester[P, S, Throwable]

//   lazy val view: GeofenceView[sys.P] = GeofenceView.fromData[S, P](sys)

//   describe("Geofence Network") {
//     it ("should run nicely") {
//       List(test1, test2, test3, test4, test5, test6, test7).foreach {
//         _ should runWithoutErrors(from = sys.empty)
//       }
//     }
//   }
// }
