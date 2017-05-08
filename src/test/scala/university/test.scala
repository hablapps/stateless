package org.hablapps.stateless
package test
package university

import org.scalatest._
import org.hablapps.puretest._, scalatestImpl._, ProgramStateMatchers.Syntax._

trait MyTest[P[_], U] extends UniversitySpec[P] { self: FunSpec with Matchers =>

  val uni: University.WithP[P, U]

  implicit val Tester: StateTester[P, U, Throwable]

  lazy val view: UniversityView[uni.P] = University.fromData[U](uni)

  describe("University") {
    it ("should execute basic bureaucracy") {
      test1 should runWithoutErrors(from = uni.create("oxford"))
      test2 should runWithoutErrors(from = uni.create("oxford"))
      test3 should runWithoutErrors(from = uni.create("oxford"))
      test4 should runWithoutErrors(from = uni.create("oxford"))
      test5 should runWithoutErrors(from = uni.create("oxford"))
    }
  }
}
