package org.hablapps.stateless
package test
package university

import org.scalatest._
import org.hablapps.puretest._, scalatestImpl._, ProgramStateMatchers.Syntax._

trait MyTest[P[_], U] extends UniversitySpec[P] { self: FunSpec with Matchers =>

  val uniSystem: System[P, U]

  lazy val MonadErrorP = uniSystem.ME

  implicit val Tester: StateTester[P, U, Throwable]

  lazy val view: UniversityView[P] =
    UniversityView.fromData[P, U](uniSystem.university)(uniSystem.ME)

  describe("University") {
    it ("should execute basic bureaucracy") {
      test1 should runWithoutErrors(from = uniSystem.university.create("oxford"))
      test2 should runWithoutErrors(from = uniSystem.university.create("oxford"))
      test3 should runWithoutErrors(from = uniSystem.university.create("oxford"))
      test4 should runWithoutErrors(from = uniSystem.university.create("oxford"))
    }
  }
}
