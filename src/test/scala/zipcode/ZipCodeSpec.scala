package org.hablapps.stateless
package test

import scalaz._, Scalaz._
import org.hablapps.puretest._
import core.nat.GetterAlg

trait ZipCodeSpec[P[_]] extends FunSpec[P] {

  implicit val M: Monad[P]
  implicit val HE: HandleError[P,Throwable]
  implicit val RE: RaiseError[P,PuretestError[Throwable]]

  val Sys: SystemData[P]; import Sys._
  val Ser: View[P]

  val Alg: DepartmentAlg.Aux[Sys.Dp, Sys.Department.Pr]
  val Lift: Alg.Person.F ~> P

  val hd = SPerson("b",Some(SAddress("c0",0)))
  val d0 = SDepartment(10,hd,List(
    SPerson("a",Some(SAddress("c1",1))),
    SPerson("c",Some(SAddress("c2",2)))))

  Describe("Department"){
    import Department._, Person._, Address._

    It("should get the budget"){
      init(d0) >>
      ((department composeLens budget).get shouldBe 10)
    }

    It("should set the budget"){
      init(d0) >>
      ((department composeLens budget).put(11)) >>
      ((department composeLens budget).get shouldBe 11)
    }

    lazy val getSPerson: Sys.Department.Person.P[SPerson] = {
      implicit lazy val M2 = Sys.Department.Person.name
      implicit lazy val M3 = Sys.Department.Person.Address.city
      for {
        nm <- name.get
        sa <- optAddress.hom(city.get >>= (c => zip.get.map(z => SAddress(c, z))))
      } yield SPerson(nm, sa)
    }

    It("should get the head"){
      init(d0) >> ((department composeLens head).hom(getSPerson) shouldBe hd)
    }

    It("should set the head"){

      val hd2 = SPerson("b2", Some(SAddress("c3", 0)))

      init(d0) *>
      Lift(Alg.Person.init(hd2)) >>=
      (pr => (department composeLens head).put(pr) *>
      ((department composeLens head).hom(getSPerson) shouldBe hd2))
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
