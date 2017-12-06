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

  val Alg: DepartmentAlg.Aux[Sys.Dp, Sys.Department.Pr, Sys.Department.Person.Ad]
  val Lift: Alg.Person.F ~> P
  val Lift2: Alg.Person.Address.F ~> P

  val ad = SAddress("c0", 0)
  val hd = SPerson("b", Some(ad))
  val d0 = SDepartment(10, hd, List(
    SPerson("a",Some(SAddress("c1",1))),
    SPerson("c",Some(SAddress("c2",2)))))

  import Department._, Person._, Address._

  lazy val getSPerson: Sys.Department.Person.P[SPerson] = {
    implicit lazy val _ = Sys.Department.Person.name
    (name.get |@| optAddress.hom(getSAddress)) { (n, a) => SPerson(n, a) }
  }

  lazy val getSAddress: Sys.Department.Person.Address.P[SAddress] = {
    implicit lazy val _ = Sys.Department.Person.Address.city
    (city.get |@| zip.get) { (c, z) => SAddress(c, z) }
  }

  lazy val setDefaultEmail: Sys.Department.Person.P[Unit] = {
    implicit lazy val _ = Sys.Department.Person.name
    name.get >>= (n => emailMap(0).set(Some(s"$n@habla.org")))
  }

  Describe("Department"){

    It("should get the budget"){
      init(d0) >>
      ((department composeLens budget).get shouldBe 10)
    }

    It("should set the budget"){
      init(d0) >>
      ((department composeLens budget).put(11)) >>
      ((department composeLens budget).get shouldBe 11)
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
      (department composeLens head composeLens name).get shouldBe "b"
    }

    It("should set info"){
      init(d0) >>
      (department composeLens head composeLens name).put("cc") >>
      (department composeLens head composeLens name).get shouldBe "cc"
    }
  }

  Describe("Department's members"){

    It("should get all members"){
      init(d0) >>
      (department composeTraversal members).hom(getSPerson).map(_.toSet) shouldBe Set(
        SPerson("b", Option(SAddress("c0", 0))),
        SPerson("a", Option(SAddress("c1", 1))),
        SPerson("c", Option(SAddress("c2", 2))))
    }
  }

  Describe("Person's email map") {

    It("should add a new email") {
      init(d0) >>
      ((department composeTraversal members).hom(setDefaultEmail)) >>
      ((department composeTraversal members).hom(emailMap(0).get)
        shouldBe List("b@habla.org", "a@habla.org", "c@habla.org").map(Some(_)))
    }
  }

  Describe("Person's address"){

    It("should get optional address"){
      init(d0) >>
      (department composeLens head composeOptional optAddress).hom(getSAddress) shouldBe Option(SAddress("c0",0))
    }

    It("should set optional address"){
      val ad2 = SAddress("c3", 3)

      init(d0) *>
      Lift2(Alg.Person.Address.init(ad2)) >>=
      (ad => (department composeLens head composeOptional optAddress).setOption(ad) *>
      ((department composeLens head composeOptional optAddress).hom(getSAddress) shouldBe Option(ad2)))
    }

    It("should get info if not none"){
      init(d0) >>
      (for {
        Some(_) <- (department composeLens head composeOptional optAddress).getOption
      } yield ())
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
