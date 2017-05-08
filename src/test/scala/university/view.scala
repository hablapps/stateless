package org.hablapps.stateless
package test
package university

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

  def getBudget(name: String): P[Option[Int]]
}
