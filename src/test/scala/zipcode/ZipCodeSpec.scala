package org.hablapps.stateless
package test

import scalaz._, Scalaz._
import org.hablapps.puretest._

trait ZipCodeSpec[P[_]] extends FunSpec[P] {

  implicit val M: Monad[P]
  implicit val HE: HandleError[P,Throwable]
  implicit val RE: RaiseError[P,PuretestError[Throwable]]

  val Sys: SystemData[P]; import Sys._
  val Ser: View[P]

  val d0 = SDepartment(10,SPerson("b",Some(SAddress("c0",0))),List(
    SPerson("a",Some(SAddress("c1",1))),
    SPerson("c",Some(SAddress("c2",2)))))

  Describe("Department"){
    It("should get the budget"){
      init(d0) >>
      ((department composeLens Department.budget).get shouldBe 10)
    }

    It("should set the budget"){
      init(d0) >>
      ((department composeLens Department.budget).put(11)) >>
      ((department composeLens Department.budget).get shouldBe 11)
    }
  }

  Describe("Department's head"){
    It("should get info"){
      init(d0) >>
      (department composeLens Department.head composeLens Department.Person.name).get shouldBe "b"
    }

    It("should set info"){
      init(d0) >>
      (department composeLens Department.head composeLens Department.Person.name).put("cc") >>
      (department composeLens Department.head composeLens Department.Person.name).get shouldBe "cc"
    }
  }

  Describe("Department's members"){
    It("should get all members"){
      init(d0) >>
      (department composeTraversal Department.members composeLens Department.Person.name).getList shouldBe
        List("b","a","c")
    }
  }

  Describe("Person's address"){
    It("should get info if not none"){
      init(d0) >>
      (for {
        Some(_) <- (department composeLens
                   Department.head composeOptional
                   Department.Person.optAddress).getOption
      } yield ()  )
    }

    It("should get all zip codes"){
      init(d0) >>
      (department composeTraversal
      Department.members composeOptional
      Department.Person.optAddress composeLens
      Department.Person.Address.zip).getList shouldBe List(0,1,2)
    }
  }

  Describe("Modify zip code"){
    It("should work"){
      init(d0) >>
      Ser.modifyZip(_+1) >>
      (department composeTraversal
      Department.members composeOptional
      Department.Person.optAddress composeLens
      Department.Person.Address.zip).getList shouldBe List(1,2,3)
    }
  }
}

trait EqualP[P[_]] {
  def apply[A: Equal]: Equal[P[A]]
}

object ZipCodeSpec{
  abstract class ScalaTest[P[_]](
    val Sys: SystemData[P],
    val Ser: View[P],
    val Tester: Tester[P,PuretestError[Throwable]])(implicit
    val M: Monad[P],
    val HE: HandleError[P,Throwable],
    val RE: RaiseError[P,PuretestError[Throwable]],
  ) extends scalatestImpl.FunSpec[P,Throwable] with ZipCodeSpec[P]
}
