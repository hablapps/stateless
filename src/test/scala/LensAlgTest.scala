package org.hablapps.stateless
package test

import org.scalatest._
import org.scalatest.prop.Checkers

import org.scalacheck.Arbitrary, Arbitrary.arbitrary

import scalacheck.StatelessProperties._

import scalaz.{ Lens => _, _ }, Scalaz._

import monocle.macros.Lenses
import monocle.Traversal

import smonocle.nat.all._

import org.hablapps.puretest._

class LensAlgTest extends FlatSpec with Matchers with Checkers {

  @Lenses
  case class Person(name: String, last: String, age: Int, address: Address)

  @Lenses
  case class Address(street: String, city: String, number: Int)

  implicit val eqPerson = Equal.equal[Person](_ == _)

  implicit val arbAddress: Arbitrary[Address] =
    Arbitrary(
      for {
        street <- arbitrary[String]
        city <- arbitrary[String]
        number <- arbitrary[Int]
      } yield Address(street, city, number))

  implicit val arbPerson: Arbitrary[Person] =
    Arbitrary(
      for {
        name <- arbitrary[String]
        last <- arbitrary[String]
        age <- arbitrary[Int]
        address <- arbitrary[Address]
      } yield Person(name, last, age, address))

  implicit val ageLn: Lens[Person, Int] = Person.age

  val john = Person("John", "Doe", 40, Address("street", "city", 1))

  "Lens" should "check" in {
    nat.lensAlg.laws[State[Person, ?], State[Int, ?], Int]
    .properties.map(_._2).foreach(check(_))
  }

  it should "get" in {
    ageLn.get.apply(john) shouldBe (john, john.age)
  }

  it should "modify" in {
    ageLn.modify(_ + 1).apply(john) shouldBe (john.copy(age = john.age + 1), ())
  }

  it should "set" in {
    ageLn.set(30).apply(john) shouldBe (john.copy(age = 30), ())
  }

  val addrLn: Lens[Person, Address] = Person.address

  val nmbLn: Lens[Address, Int] = Address.number

  it should "compose" in {

    val ln = addrLn composeLens nmbLn

    ln.get.apply(john) shouldBe (john, john.address.number)

    ln.modify(_ + 1).apply(john) shouldBe (john.copy(
      address = john.address.copy(
        number = john.address.number + 1)), ())

    ln.set(3).apply(john) shouldBe (john.copy(
      address = john.address.copy(number = 3)), ())
  }

  val txtTr = fromTraversal[Id, Address, String](new Traversal[Address, String] {
    def modifyF[F[_]: Applicative](f: String => F[String])(s: Address): F[Address] =
      (f(s.street) |@| f(s.city)) { Address(_, _, s.number) }
  })

  it should "compose with Traversals" in {
    val tr = addrLn composeTraversal txtTr

    tr.getList.apply(john) shouldBe
      (john, List(john.address.street, john.address.city))

    tr.modify(_.toUpperCase).apply(john) shouldBe
      (john.copy(address = john.address.copy(
        street = john.address.street.toUpperCase,
        city = john.address.city.toUpperCase)), ())

    tr.set("*").apply(john) shouldBe
    (john.copy(address = john.address.copy(street = "*", city = "*")), ())
  }
}
