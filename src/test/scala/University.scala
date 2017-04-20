package org.hablapps.stateless
package test

import org.scalatest._

import scalaz.State

import monocle.macros.GenLens
import monocle.function.all._
import monocle.std.string._
import monocle.{ Traversal => MTraversal }

import smonocle.nat.all._
import core.nat.op.At.syntax._

// This example has been extracted and adapted from:
// https://julien-truffaut.github.io/Monocle/examples/university_example.html
class University extends FlatSpec with Matchers {

  case class Lecturer(firstName: String, lastName: String, salary: Int)
  case class Department(budget: Int, lecturers: List[Lecturer])
  case class University(name: String, departments: Map[String, Department])

  val uni = University("oxford", Map(
    "Computer Science" -> Department(45, List(
      Lecturer("john"  , "doe", 10),
      Lecturer("robert", "johnson", 16)
    )),
    "History" -> Department(30, List(
      Lecturer("arnold", "stones", 20)
    ))
  ))

  // How to remove or add elements in a Map

  val departments: Lens[University, Map[String, Department]] =
    GenLens[University](_.departments)

  def department(s: String): Lens[University, Option[Department]] =
    departments.composeLens[State[Option[Department], ?], Option[Department]](
      fromIMapState.at(s))

  "Optics" should "remove elements from a map" in {
    department("History").set(None).exec(uni) shouldBe
      uni.copy(departments = uni.departments - "History")
  }

  it should "add new elements into the map" in {
    val physics = Department(36, List(
      Lecturer("daniel", "jones", 12),
      Lecturer("roger" , "smith", 14)
    ))

    department("Physics").set(Some(physics)).exec(uni) shouldBe
      uni.copy(departments = uni.departments + ("Physics" -> physics))
  }

  // How to update a field in a nested case class

  val lecturers: Lens[Department, List[Lecturer]] = GenLens[Department](_.lecturers)
  val salary: Lens[Lecturer, Int] = GenLens[Lecturer](_.salary)
  val allLecturers: Traversal[University, Lecturer] = departments
    .composeTraversal[State[Department, ?], Department](asTraversal(each))
    .composeLens[State[List[Lecturer], ?], List[Lecturer]](lecturers)
    .composeTraversal[State[Lecturer, ?], Lecturer](asTraversal(each))

  it should "update a field in nested class" in {
    allLecturers.composeLens[State[Int, ?], Int](salary).modify(_ + 2).exec(uni) shouldBe
      University("oxford", Map(
        "Computer Science" -> Department(45, List(
          Lecturer("john"  , "doe", 10 + 2),
          Lecturer("robert", "johnson", 16 + 2)
        )),
        "History" -> Department(30, List(
          Lecturer("arnold", "stones", 20 + 2)
        ))
      ))
  }

  // How to create your own traversal

  val firstName: Lens[Lecturer, String] = GenLens[Lecturer](_.firstName)
  val lastName: Lens[Lecturer, String] = GenLens[Lecturer](_.lastName)
  val allFirstNameHead: Traversal[University, Char] = allLecturers
    .composeLens[State[String, ?], String](firstName)
    .composeOptional[State[Char, ?], Char](asOptional(headOption))
  val allLastNameHead: Traversal[University, Char] = allLecturers
    .composeLens[State[String, ?], String](lastName)
    .composeOptional[State[Char, ?], Char](asOptional(headOption))

  it should "upper case first name and then last name" in {
    val upperCasedFirstName = allFirstNameHead.modify(_.toUpper).exec(uni)

    upperCasedFirstName shouldBe
      University("oxford", Map(
        "Computer Science" -> Department(45, List(
          Lecturer("John"  , "doe", 10),
          Lecturer("Robert", "johnson", 16)
        )),
        "History" -> Department(30, List(
          Lecturer("Arnold", "stones", 20)
        ))
      ))

    allLastNameHead.modify(_.toUpper).exec(upperCasedFirstName) shouldBe
      University("oxford", Map(
        "Computer Science" -> Department(45, List(
          Lecturer("John"  , "Doe", 10),
          Lecturer("Robert", "Johnson", 16)
        )),
        "History" -> Department(30, List(
          Lecturer("Arnold", "Stones", 20)
        ))
      ))
  }

  val firstAndLastNames: Traversal[Lecturer, String] =
    MTraversal.apply2[Lecturer, String](_.firstName, _.lastName) {
      case (fn, ln, l) => l.copy(firstName = fn, lastName = ln)
    }
  val allFirstAndLastNamesHead: Traversal[University, Char] = allLecturers
    .composeTraversal[State[String, ?], String](firstAndLastNames)
    .composeOptional[State[Char, ?], Char](asOptional(headOption))

  it should "upper case first and last name all together" in {
    allFirstAndLastNamesHead.modify(_.toUpper).exec(uni) shouldBe
      University("oxford", Map(
        "Computer Science" -> Department(45, List(
          Lecturer("John"  , "Doe", 10),
          Lecturer("Robert", "Johnson", 16)
        )),
        "History" -> Department(30, List(
          Lecturer("Arnold", "Stones", 20)
        ))
      ))
  }
}
