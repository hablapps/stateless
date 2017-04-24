package org.hablapps.stateless
package test

import org.scalatest._

import scalaz.State
import scalaz.syntax.std.option._

import core.nat._, op.At
import smonocle.nat.all._

class University2 extends FlatSpec with Matchers {

  /* Data */

  trait University[P[_], U] {
    type DP[_]
    type D

    val name: LensField[P, String]
    val department: Department[DP, D]
    val departments: TraversalAlg.Aux[P, DP, D]
    val ev: At[P, DP, String, D]
  }

  trait Department[P[_], D] {
    type LP[_]
    type L

    val lecturer: Lecturer[LP, L]
    val lecturers: TraversalAlg.Aux[P, LP, L]
    val budget: LensField[P, Int]

    def create(b: Int, l: List[(String, String, Int)]): D
  }

  trait Lecturer[P[_], L] {
    val first: LensField[P, String]
    val last: LensField[P, String]
    val salary: LensField[P, Int]

    def create(f: String, l: String, s: Int): L
  }

  /* View */

  trait TestView[P[_]] {

    def removeDepartment(name: String): P[Unit]

    def addDepartment(
      name: String,
      budget: Int,
      lecturers: List[(String, String, Int)]): P[Unit]

    def updateSalary(f: Int => Int): P[Unit]

    def upperCase: P[Unit]
  }

  object TestUniversity {

    def fromData[P[_], U](university: University[P, U]) = new TestView[P] {

      def removeDepartment(name: String): P[Unit] =
        university.ev.at(name).set(None)

      def addDepartment(
          name: String,
          budget: Int,
          lecturers: List[(String, String, Int)]): P[Unit] =
        university.ev
          .at(name)
          .set(university.department.create(budget, lecturers).some)

      def updateSalary(f: Int => Int): P[Unit] = {
        import university._, department._, lecturer._
        departments
          .composeTraversal(lecturers)
          .composeLens(salary)
          .modify(f)
      }

      def upperCase: P[Unit] = {
        import university._, department._, lecturer._, lecturers.ev
        departments
          .composeTraversal(lecturers)
          .composeTraversal(first parLens last)
          .modify(_.toUpperCase)
      }
    }
  }
}
