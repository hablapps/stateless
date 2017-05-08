package org.hablapps.stateless
package test
package university

import scalaz.MonadError
import scalaz.syntax.monad._

import org.hablapps.puretest._, Filter.Syntax._

trait UniversitySpec[P[_]] {

  /* To be tested */

  val view: UniversityView[P]

  /* Dependencies */

  implicit val MonadErrorP: MonadError[P, Throwable]
  implicit val FilterP: Filter[P]

  /* Tests */

  def prefix: P[Unit] =
    for {
      _ <- view.addDepartment("Computer Science", 45, List(
        ("john"  , "doe", 10),
        ("robert", "johnson", 16)))
      _ <- view.addDepartment("History", 30, List(
        ("arnold", "stones", 20)))
    } yield ()

  // Test 1: removing an existing department

  def test1: P[Unit] =
    for {
          _ <- prefix
          _ <- view.removeDepartment("History")
      false <- view.containsDepartment("History")
    } yield ()

  // Test 2: adding a physics department

  def test2: P[Unit] =
    for {
         _ <- prefix
         _ <- view.addDepartment("Physics", 36, List(
            ("daniel", "jones", 12),
            ("roger" , "smith", 14)))
      true <- view.containsDepartment("Physics")
  } yield ()

  // Test 3: updating salaries

  def test3: P[Unit] =
    for {
       _ <- prefix
      v1 <- view.getTotalSalary
       _ <- view.updateSalary(_ * 2) // not bad
       _ <- view.getTotalSalary.assertEqual(v1 * 2)
    } yield ()

  // Test 4: formatting names

  def test4: P[Unit] =
    for {
         _ <- prefix
         _ <- view.upperCase
      true <- view.getNameInitials.map(_.forall(_.isUpper))
    } yield ()

  // Test 5: picking department

  def test5: P[Unit] =
    for {
             _ <- prefix
      Some(45) <- view.getBudget("Computer Science")
          None <- view.getBudget("Mathematics")
    } yield ()
}
