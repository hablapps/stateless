package org.hablapps.stateless
package test

import scalaz._, Scalaz._
import org.hablapps.puretest._

trait ZipCodeSpec[P[_]] extends FunSpec[P]{

  implicit val M: Monad[P]
  implicit val HE: HandleError[P,Throwable]
  implicit val RE: RaiseError[P,PuretestError[Throwable]]

  val Sys: SystemData[P]; import Sys._
  val Ser: View[P]

  val d0 = SDepartment(10,SPerson("b",None),List(
    SPerson("a",Some(SAddress("c1",1))),
    SPerson("b",None),
    SPerson("c",Some(SAddress("c2",2)))))

  // Describe("Modify zip code"){
  //   It("should work"){
  //     init(d0) >>
  //     Ser.modifyZip(_+1) >>
  //     (department composeTraversal
  //     Department.members composeOptional
  //     Department.Person.optAddress composeLens
  //     Department.Person.Address.zip).getList shouldBe List(2,3)
  //   }
  // }

  Describe("Department"){
    It("should be ruled by 'the boss'"){
      init(d0) >>
      (department composeLens Department.head composeLens Department.Person.name).get shouldBe "b"
    }

    It("should have correct budget"){
      init(d0) >>
      (department composeLens Department.budget).get shouldBe 10
    }
  }
}

object ZipCodeSpec{
  class ScalaTest[P[_]](
    val Sys: SystemData[P],
    val Ser: View[P],
    val Tester: Tester[P,PuretestError[Throwable]])(implicit
    val M: Monad[P],
    val HE: HandleError[P,Throwable],
    val RE: RaiseError[P,PuretestError[Throwable]]
  ) extends scalatestImpl.FunSpec[P,Throwable] with ZipCodeSpec[P]
}