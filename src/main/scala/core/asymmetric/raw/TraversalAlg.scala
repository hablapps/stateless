package org.hablapps.phoropter
package core
package asymmetric
package raw

import scalaz.{ Monad, Monoid }
import scalaz.syntax.foldable._
import scalaz.std.list._

trait TraversalAlg[P[_], A] extends Monad[P] {

  def getAll: P[List[A]]

  def modify(f: A => A): P[Unit]

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

  def set(a: A): P[Unit] = modify(_ => a)
}
