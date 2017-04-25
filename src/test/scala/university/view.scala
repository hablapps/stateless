package org.hablapps.stateless
package test
package university

import scalaz._, Scalaz._

import monocle.function.all._

import smonocle.nat.all.asOptional

trait UniversityView[P[_]] {

  def removeDepartment(name: String): P[Unit]

  def addDepartment(
    name: String,
    budget: Int,
    lecturers: List[(String, String, Int)]): P[Unit]

  def updateSalary(f: Int => Int): P[Unit]

  def upperCase: P[Unit]

  def containsDepartment(name: String): P[Boolean]

  def getTotalSalary: P[Int]

  def getNameInitials: P[List[Char]]
}

object UniversityView {

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
