package org.hablapps.phoropter
package test

import org.scalatest._

import scalaz._, Scalaz._

import monocle.macros.Lenses
import monocle.Traversal

import org.hablapps.phoropter.smonocle.asymmetric.nat.all._

class LensAlgTest extends FlatSpec with Matchers {

  @Lenses
  case class Person(name: String, last: String, age: Int, address: Address)

  @Lenses
  case class Address(street: String, city: String, number: Int)

  val ageLn = fromLens[Id, Person, Int](Person.age)

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

  val addrLn = fromLens[Id, Person, Address](Person.address)

  val nmbLn = fromLens[Id, Address, Int](Address.number)

  it should "compose" in {

    val ln = addrLn.composeLens[StateT[Id, Int, ?], Int](nmbLn)

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
    val tr = addrLn.composeTraversal[StateT[Id, String, ?], String](txtTr)

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
