package org.hablapps.phoropter
package core
package test

import org.scalatest._
import scalaz.{ Lens => _, _ }, Scalaz._

class LensTest extends FlatSpec with Matchers {

  case class Person(name: String, last: String, age: Int)

  val ageLn = new Lens[State[Person, ?], State[Int, ?], Int] {
    def modifyF[O](f: State[Int, O]): State[Person, O] =
      State(p => (p.copy(age = f(p.age)._1), f(p.age)._2))
  }

  val john = Person("John", "Doe", 40)

  "Lens" should "get" in {
    ageLn.get.apply(john) shouldBe (john, john.age)
  }

  it should "modify" in {
    ageLn.modify(_ + 1).apply(john) shouldBe (john.copy(age = john.age + 1), ())
  }

  it should "set" in {
    ageLn.set(30).apply(john) shouldBe (john.copy(age = 30), ())
  }
}
