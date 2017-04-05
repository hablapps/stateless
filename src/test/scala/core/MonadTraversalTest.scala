package org.hablapps.phoropter
package test

import org.scalatest._

import scalaz.{ Lens => _, _ }, Scalaz._

import monocle.Traversal

import org.hablapps.phoropter.state.all._

class MonadTraversalTest extends FlatSpec with Matchers {

  case class Person(name: String, last: String, age: Int)

  val txtTr = fromTraversal[Id, Person, String](new Traversal[Person, String] {
    def modifyF[F[_]: Applicative](f: String => F[String])(s: Person): F[Person] =
      (f(s.name) |@| f(s.last)) { Person(_, _, s.age) }
  })

  val john = Person("John", "Doe", 40)

  "Traversal" should "getAll" in {
    txtTr.getAll.apply(john) shouldBe (john, List("John", "Doe"))
  }

  it should "modify" in {
    txtTr.modify(_.toUpperCase).apply(john) shouldBe
      (john.copy(name = john.name.toUpperCase, last = john.last.toUpperCase), ())
  }

  it should "set" in {
    txtTr.set("*").apply(john) shouldBe (john.copy(name = "*", last = "*"), ())
  }

  it should "count" in {
    txtTr.count.apply(john) shouldBe (john, 2)
  }
}
