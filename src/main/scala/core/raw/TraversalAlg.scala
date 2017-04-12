package org.hablapps.stateless
package core
package raw

import scalaz.{ Equal, Monad, Monoid }
import scalaz.syntax.equal._
import scalaz.syntax.foldable._
import scalaz.syntax.monad._
import scalaz.std.list._

trait TraversalAlg[P[_], A] extends Monad[P] { self =>

  def getList: P[List[A]]

  def modifyList(f: A => A): P[List[Unit]]

  /* derived methods */

  def foldMap[M: Monoid](f: A => M): P[M] = map(getList)(_.foldMap(f))

  def find(p: A => Boolean): P[Option[A]] = map(getList)(_.find(p))

  def headOption: P[Option[A]] = map(getList)(_.headOption)

  def lastOption: P[Option[A]] = map(getList)(_.lastOption)

  def exist(p: A => Boolean): P[Boolean] = map(getList)(_.exists(p))

  def all(p: A => Boolean): P[Boolean] = map(getList)(_.all(p))

  def length: P[Int] = map(getList)(_.length)

  def isEmpty: P[Boolean] = map(getList)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(isEmpty)(! _)

  def setList(a: A): P[List[Unit]] = modifyList(_ => a)

  def set(a: A): P[Unit] = void(setList(a))

  def modify(f: A => A): P[Unit] = void(modifyList(f))

  trait TraversalAlgLaw {
    implicit val _: Monad[P] = self

    def getGet(implicit eq: Equal[P[(List[A], List[A])]]): Boolean =
      (getList >>= (as1 => getList >>= (as2 => (as1, as2).point[P]))) ===
        (getList >>= (as => (as, as).point[P]))

    def putGet(a: A)(implicit eq: Equal[P[List[A]]]): Boolean =
      (setList(a) >> getList) === (setList(a).map(_.as(a)))

    def putPut(a1: A, a2: A)(implicit eq: Equal[P[List[Unit]]]): Boolean =
      (setList(a1) >> setList(a2)) === setList(a2)
  }
}
