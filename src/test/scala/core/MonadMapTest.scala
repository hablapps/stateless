package org.hablapps.phoropter
package test

import org.scalatest._

import scalaz._, Scalaz._

import org.hablapps.phoropter.state.all._

class MonadMapTest extends FlatSpec with Matchers {

  case class Person(name: String, last: String, age: Int)

  val map = Map(
    "0" -> Person("John", "Doe", 40),
    "1" -> Person("John", "Wick", 50))

  val myMap = fromMap[Id, String, Person]

  "Map" should "getAll" in {
    myMap.getAll(map) shouldBe (map, map.toList)
  }

  it should "set" in {
    val deere = Person("John", "Deere", 60)
    myMap.set(deere)(map) shouldBe ((map.map(_ map (_ => deere)), ()))
  }

  it should "modify" in {
    val f: Person => Person = _.copy(name = "James")
    myMap.modify(f)(map) shouldBe ((map.map(_ map f), ()))
  }

  it should "indexes" in {
    myMap.indexes(map) shouldBe (map, map.keys.toList)
  }

  it should "foci" in {
    myMap.foci(map) shouldBe (map, map.values.toList)
  }

  it should "collect" in {
    myMap.collect(StateT[Id, Person, String](s => (s, "out")))(map) shouldBe
      (map, List.fill(map.size)("out"))
  }

  it should "add" in {
    val rambo = Person("John", "Rambo", 45)
    myMap.add("2")(rambo)(map) shouldBe ((map + ("2" -> rambo), ()))
  }

  it should "remove" in {
    myMap.remove("1")(map) shouldBe ((map - "1", ()))
  }

  it should "get" in {
    myMap.get("1")(map) shouldBe (map, Person("John", "Wick", 50).some)
  }
}
