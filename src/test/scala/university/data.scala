package org.hablapps.stateless
package test
package university

import scalaz.{ Monad, MonadError }

import core.nat._, op.At
import smonocle.nat.all._

trait University[P[_], U] {
  type AP[_]
  type DP[_]
  type D

  val name: LensField[P, String]
  val department: Department[DP, D]
  val departments: TraversalAlg.Aux[P, DP, D]
  val ev: At[P, AP, String, D]

  def create(name: String): U
}

object University {

  type Aux[P[_], U, AP2[_], DP2[_], D2] = University[P, U] {
    type AP[x] = AP2[x]
    type DP[x] = DP2[x]
    type D = D2
  }

  def apply[P[_], U, AP2[_], DP2[_], D2](
      name2: LensField[P, String],
      department2: Department[DP2, D2],
      departments2: TraversalAlg.Aux[P, DP2, D2],
      ev2: At[P, AP2, String, D2],
      create2: String => U): Aux[P, U, AP2, DP2, D2] =
    new University[P, U] {
      type AP[x] = AP2[x]
      type DP[x] = DP2[x]
      type D = D2
      val name = name2
      val department = department2
      val departments = departments2
      val ev = ev2
      def create(name: String) = create2(name)
    }

  import scalaz._, Scalaz._
  import monocle.function.all._
  import smonocle.nat.all.asOptional

  def fromData[P[_]: Monad, U](uni: University[P, U]) = new UniversityView[P] {
    import uni._, department._, lecturer._

    def removeDepartment(name: String): P[Unit] =
      uni.ev.at(name).set(None)

    def addDepartment(
        name: String,
        budget: Int,
        lecturers: List[(String, String, Int)]): P[Unit] =
      uni.ev.at(name).set(uni.department.create(budget, lecturers).some)

    def updateSalary(f: Int => Int): P[Unit] =
      salaryTraversal.modify(f)

    def upperCase: P[Unit] =
      headNameTraversal.modify(_.toUpper)

    def containsDepartment(name: String): P[Boolean] =
      uni.ev.at(name).gets(_.nonEmpty)

    def getTotalSalary: P[Int] =
      salaryTraversal.foldMap(identity)

    def getNameInitials: P[List[Char]] =
      headNameTraversal.getList

    private val salaryTraversal =
      departments
        .composeTraversal(lecturers)
        .composeLens(salary)

    private val headNameTraversal =
      departments
        .composeTraversal(lecturers)
        .composeTraversal(first parLens last)
        .composeOptional(asOptional(headOption))
  }
}

trait Department[P[_], D] {
  type LP[_]
  type L

  val lecturer: Lecturer[LP, L]
  val lecturers: TraversalAlg.Aux[P, LP, L]
  val budget: LensField[P, Int]

  def create(b: Int, l: List[(String, String, Int)]): D
}

object Department {

  type Aux[P[_], D, LP2[_], L2] = Department[P, D] {
    type LP[x] = LP2[x]
    type L = L2
  }

  def apply[P[_], D, LP2[_], L2](
      lecturer2: Lecturer[LP2, L2],
      lecturers2: TraversalAlg.Aux[P, LP2, L2],
      budget2: LensField[P, Int],
      create2: (Int, List[(String, String, Int)]) => D): Aux[P, D, LP2, L2] =
    new Department[P, D] {
      type LP[x] = LP2[x]
      type L = L2
      val lecturer = lecturer2
      val lecturers = lecturers2
      val budget = budget2
      def create(b: Int, l: List[(String, String, Int)]) = create2(b, l)
    }
}

trait Lecturer[P[_], L] {
  val first: LensField[P, String]
  val last: LensField[P, String]
  val salary: LensField[P, Int]

  def create(f: String, l: String, s: Int): L
}

object Lecturer {

  def apply[P[_], L](
      first2: LensField[P, String],
      last2: LensField[P, String],
      salary2: LensField[P, Int],
      create2: (String, String, Int) => L): Lecturer[P, L] =
    new Lecturer[P, L] {
      val first = first2
      val last = last2
      val salary = salary2
      def create(f: String, l: String, s: Int) = create2(f, l, s)
    }
}
