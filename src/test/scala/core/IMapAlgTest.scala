package org.hablapps.phoropter
package test

import org.scalatest._

import scalaz._, Scalaz._

import org.hablapps.phoropter.smonocle.asymmetric.nat.all._

class IMapAlgTest extends FlatSpec with Matchers {

  case class Person(name: String, last: String, age: Int)

  val doe = Person("John", "Doe", 40)
  val wick = Person("John", "Wick", 50)

  val init = Map("0" -> doe, "1" -> wick)

  val myMap = fromMap[Id, String, Person]

  "Map" should "getList" in {
    myMap.getList(init) shouldBe (init, init.toList)
  }

  it should "set" in {
    val deere = Person("John", "Deere", 60)
    myMap.set(deere)(init) shouldBe ((init.map(_ map (_ => deere)), ()))
  }

  it should "modify" in {
    val f: Person => Person = _.copy(name = "James")
    myMap.modify(f)(init) shouldBe ((init.map(_ map f), ()))
  }

  it should "indexes" in {
    myMap.indexes(init) shouldBe (init, init.keys.toList)
  }

  it should "foci" in {
    myMap.foci(init) shouldBe (init, init.values.toList)
  }

  it should "collect" in {
    myMap.collect(StateT[Id, Person, String](s => (s, "out")))(init) shouldBe
      (init, List.fill(init.size)("out"))
  }

  it should "add" in {
    val rambo = Person("John", "Rambo", 45)
    myMap.add("2")(rambo)(init) shouldBe ((init + ("2" -> rambo), ()))
  }

  it should "remove" in {
    myMap.remove("1")(init) shouldBe ((init - "1", ()))
  }

  it should "pick" in {
    val lennon = Person("John", "Lennon", 30)
    myMap.pick("0")(State.put(lennon.some))(init) shouldBe
      ((init.updated("0", lennon), ()))
    myMap.pick("1")(State.modify(
        _.fold(Option.empty[Person])(_.copy(last = "McEnroe").some)))(init) shouldBe
      ((init.updated("1", wick.copy(last = "McEnroe")), ()))
    myMap.pick("2")(State.put(lennon.some))(init) shouldBe
      ((init + ("2" -> lennon), ()))
  }

  it should "get" in {
    myMap.get("1")(init) shouldBe (init, wick.some)
  }
}
