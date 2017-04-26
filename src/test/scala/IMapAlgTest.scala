package org.hablapps.stateless
package test

import org.scalatest._

import scalaz._, Scalaz._

import shapeless.{ Id => _, _ }

import smonocle.nat.all._

class IMapAlgTest extends FlatSpec with Matchers {

  case class Person(name: String, last: String, age: Int)

  val doe = Person("John", "Doe", 40)
  val wick = Person("John", "Wick", 50)

  val init = Map(("0" :: HNil) -> doe, ("1" :: HNil) -> wick)

  val myMap = fromMap[Id, String :: HNil, Person]

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
    myMap.add("2" :: HNil)(rambo)(init) shouldBe ((init + (("2" :: HNil) -> rambo), ()))
  }

  it should "remove" in {
    myMap.remove("1" :: HNil)(init) shouldBe ((init - ("1" :: HNil), ()))
  }

  it should "pick" in {
    val lennon = Person("John", "Lennon", 30)
    myMap.pick("0" :: HNil)(State.put(lennon.some))(init) shouldBe
      ((init.updated("0" :: HNil, lennon), ()))
    myMap.pick("1" :: HNil)(State.modify(
        _.fold(Option.empty[Person])(_.copy(last = "McEnroe").some)))(init) shouldBe
      ((init.updated("1" :: HNil, wick.copy(last = "McEnroe")), ()))
    myMap.pick("2" :: HNil)(State.put(lennon.some))(init) shouldBe
      ((init + (("2" :: HNil) -> lennon), ()))
  }

  it should "get" in {
    myMap.get("1" :: HNil)(init) shouldBe (init, wick.some)
  }
}
