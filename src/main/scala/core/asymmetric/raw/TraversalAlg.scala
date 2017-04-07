package org.hablapps.phoropter
package core
package asymmetric
package raw

import scalaz.{ Equal, Monad, Monoid }
import scalaz.syntax.equal._
import scalaz.syntax.foldable._
import scalaz.syntax.monad._
import scalaz.std.list._

trait TraversalAlg[P[_], A] extends Monad[P] { self =>

  def getAll: P[List[A]]

  def modifyList(f: A => A): P[List[Unit]]

  /* derived methods */

  def foldMap[M: Monoid](f: A => M): P[M] = map(getAll)(_.foldMap(f))

  def find(p: A => Boolean): P[Option[A]] = map(getAll)(_.find(p))

  def headOption: P[Option[A]] = map(getAll)(_.headOption)

  def lastOption: P[Option[A]] = map(getAll)(_.lastOption)

  def exist(p: A => Boolean): P[Boolean] = map(getAll)(_.exists(p))

  def all(p: A => Boolean): P[Boolean] = map(getAll)(_.all(p))

  def length: P[Int] = map(getAll)(_.length)

  def isEmpty: P[Boolean] = map(getAll)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(isEmpty)(! _)

  def setList(a: A): P[List[Unit]] = modifyList(_ => a)

  def set(a: A): P[Unit] = map(setList(a))(_ => ())

  def modify(f: A => A): P[Unit] = map(modifyList(f))(_ => ())

  trait TraversalAlgLaw {
    implicit val _: Monad[P] = self

    def getGet(implicit eq: Equal[P[(List[A], List[A])]]): Boolean =
      (getAll >>= (as1 => getAll >>= (as2 => (as1, as2).point[P]))) ===
        (getAll >>= (as => (as, as).point[P]))

    def putGet(a: A)(implicit eq: Equal[P[List[A]]]): Boolean =
      (setList(a) >> getAll) === (setList(a).map(_.as(a)))

    def putPut(a1: A, a2: A)(implicit eq: Equal[P[List[Unit]]]): Boolean =
      (setList(a1) >> setList(a2)) === setList(a2)
  }
}
