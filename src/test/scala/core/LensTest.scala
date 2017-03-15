package org.hablapps.phoropter
package core
package test

import org.scalatest._
import scalaz.{ Lens => _, _ }, Scalaz._

class LensTest extends FlatSpec with Matchers {

  case class Person(name: String, last: String, age: Int, address: Address)

  case class Address(street: String, city: String, number: Int)

  val ageLn = new Lens[State[Person, ?], State[Int, ?], Int] {
    implicit val MS: MonadState[State[Int, ?], Int] =
      StateT.stateTMonadState[Int, Id.Id]
    def modifyF[O](f: State[Int, O]): State[Person, O] =
      State(p => (p.copy(age = f(p.age)._1), f(p.age)._2))
  }

  val john = Person("John", "Doe", 40, Address("street", "city", 1))

  "Lens" should "get" in {
    ageLn.get.apply(john) shouldBe (john, john.age)
  }

  it should "modify" in {
    ageLn.modify(_ + 1).apply(john) shouldBe (john.copy(age = john.age + 1), ())
  }

  it should "set" in {
    ageLn.set(30).apply(john) shouldBe (john.copy(age = 30), ())
  }

  val addrLn = new Lens[State[Person, ?], State[Address, ?], Address] {
    implicit val MS: MonadState[State[Address, ?], Address] =
      StateT.stateTMonadState[Address, Id.Id]
    def modifyF[O](f: State[Address, O]): State[Person, O] =
      State(p => (p.copy(address = f(p.address)._1), f(p.address)._2))
  }

  val nmbLn = new Lens[State[Address, ?], State[Int, ?], Int] {
    implicit val MS: MonadState[State[Int, ?], Int] =
      StateT.stateTMonadState[Int, Id.Id]
    def modifyF[O](f: State[Int, O]): State[Address, O] =
      State(a => (a.copy(number = f(a.number)._1), f(a.number)._2))
  }

  it should "compose" in {
    val ln = addrLn.compose[State[Int, ?], Int](nmbLn)

    ln.get.apply(john) shouldBe (john, john.address.number)

    ln.modify(_ + 1).apply(john) shouldBe (john.copy(
      address = john.address.copy(
        number = john.address.number + 1)), ())

    ln.set(3).apply(john) shouldBe (john.copy(
      address = john.address.copy(number = 3)), ())
  }

  val txtTr = new Traversal[State[Address, ?], State[String, ?], String] {
    implicit val MS: MonadState[State[String, ?], String] =
      StateT.stateTMonadState[String, Id.Id]
    def modifyF[O: Monoid](
        qo: State[String, O]): State[Address, O] = State { a =>
      val (street2, os) = qo(a.street)
      val (city2, oc) = qo(a.city)
      (a.copy(street = street2, city = city2), os |+| oc)
    }
  }

  it should "compose with Traversals" in {
    val tr = addrLn.compose[State[String, ?], String](txtTr)

    tr.getAll.apply(john) shouldBe
      (john, List(john.address.street, john.address.city))

    tr.modify(_.toUpperCase).apply(john) shouldBe
      (john.copy(address = john.address.copy(
        street = john.address.street.toUpperCase,
        city = john.address.city.toUpperCase)), ())

    tr.set("*").apply(john) shouldBe
    (john.copy(address = john.address.copy(street = "*", city = "*")), ())
  }
}
