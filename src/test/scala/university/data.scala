package org.hablapps.stateless
package test
package university

import scalaz.{ Monad, MonadError }

import core.nat._, op.At
import smonocle.nat.all._

trait System[P[_], U] {

  val university: University[P, U]

  implicit val ME: MonadError[P, Throwable]
}

object System {
  def apply[P[_], U](
      uni2: University[P, U])(implicit
      ME2: MonadError[P, Throwable]): System[P, U] =
    new System[P, U] {
      val university = uni2
      implicit val ME = ME2
    }
}

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
