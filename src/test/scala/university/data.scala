package org.hablapps.stateless
package test
package university

import scalaz._, Scalaz._
import monocle.function.all._

import core.nat._, op.At, lib.MapAlg
import smonocle.nat.all._

trait University[U] {
  type P[_]
  type D

  val name: LensField[P, String]
  val department: Department[D]
  val departments: MapAlg.Aux[P, department.P, String, D]

  def create(name: String): U
}

object University {

  type WithP[P2[_], U] = University[U] { type P[x] = P2[x] }

  type Aux[P2[_], U, D2] = University[U] {
    type P[x] = P2[x]
    type D = D2
  }

  def apply[P2[_], U, DP2[_], D2](
      name2: LensField[P2, String],
      department2: Department.WithP[DP2, D2],
      departments2: MapAlg.Aux[P2, DP2, String, D2],
      create2: String => U): Aux[P2, U, D2] =
    new University[U] {
      type P[x] = P2[x]
      type D = D2
      val name = name2
      val department = department2
      val departments = departments2
      def create(name: String) = create2(name)
    }

  def fromData[U](uni: University[U]) = new UniversityView[uni.P] {
    import uni._, department.{ P => _, _ }, lecturer.{ P => _, _ }

    def removeDepartment(name: String): P[Unit] =
      uni.departments(name).set(None)

    def addDepartment(
        name: String,
        budget: Int,
        lecturers: List[(String, String, Int)]): P[Unit] =
      uni.departments(name).set(uni.department.create(budget, lecturers).some)

    def updateSalary(f: Int => Int): P[Unit] =
      salaryTraversal.modify(f)

    def upperCase: P[Unit] =
      headNameTraversal.modify(_.toUpper)

    def containsDepartment(name: String): P[Boolean] =
      uni.departments(name).gets(_.nonEmpty)

    def getTotalSalary: P[Int] =
      salaryTraversal.foldMap(identity)

    def getNameInitials: P[List[Char]] =
      headNameTraversal.foci

    private val salaryTraversal =
      departments.tr
        .composeITraversal(lecturers.asIndexed)
        .composeILens(salary.asIndexed)

    private val headNameTraversal =
      departments.tr
        .composeITraversal(lecturers.asIndexed)
        .composeITraversal((first parLens last).asIndexed)
        .composeIOptional(asOptional(headOption[String, Char]).asIndexed)
  }
}

trait Department[D] {
  type P[_]
  type L

  val lecturer: Lecturer[L]
  val lecturers: TraversalAlg.Aux[P, lecturer.P, L]
  val budget: LensField[P, Int]

  def create(b: Int, l: List[(String, String, Int)]): D
}

object Department {

  type WithP[P2[_], D] = Department[D] { type P[x] = P2[x] }

  type Aux[P2[_], D, L2] = Department[D] {
    type P[x] = P2[x]
    type L = L2
  }

  def apply[P2[_], D, LP2[_], L2](
      lecturer2: Lecturer.WithP[LP2, L2],
      lecturers2: TraversalAlg.Aux[P2, LP2, L2],
      budget2: LensField[P2, Int],
      create2: (Int, List[(String, String, Int)]) => D): Aux[P2, D, L2] =
    new Department[D] {
      type P[x] = P2[x]
      type L = L2
      val lecturer = lecturer2
      val lecturers = lecturers2
      val budget = budget2
      def create(b: Int, l: List[(String, String, Int)]) = create2(b, l)
    }
}

trait Lecturer[L] {
  type P[_]

  val first: LensField[P, String]
  val last: LensField[P, String]
  val salary: LensField[P, Int]

  def create(f: String, l: String, s: Int): L
}

object Lecturer {

  type WithP[P2[_], L] = Lecturer[L] { type P[x] = P2[x] }

  type Aux[P[_], L] = WithP[P, L]

  def apply[P2[_], L](
      first2: LensField[P2, String],
      last2: LensField[P2, String],
      salary2: LensField[P2, Int],
      create2: (String, String, Int) => L): Aux[P2, L] =
    new Lecturer[L] {
      type P[x] = P2[x]
      val first = first2
      val last = last2
      val salary = salary2
      def create(f: String, l: String, s: Int) = create2(f, l, s)
    }
}
