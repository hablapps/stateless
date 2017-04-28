package org.hablapps.stateless
package core
package raw

import scalaz.{ Monad, Monoid }
import scalaz.syntax.foldable._
import scalaz.std.list._

trait IFoldAlg[P[_], I, A] extends Monad[P] {

  def getList: P[List[(I, A)]]

  /* derived methods */

  def foldMap[M: Monoid](f: ((I, A)) => M): P[M] = map(getList)(_.foldMap(f))

  def find(p: ((I, A)) => Boolean): P[Option[(I, A)]] = map(getList)(_.find(p))

  def headOption: P[Option[(I, A)]] = map(getList)(_.headOption)

  def lastOption: P[Option[(I, A)]] = map(getList)(_.lastOption)

  def exist(p: ((I, A)) => Boolean): P[Boolean] = map(getList)(_.exists(p))

  def all(p: ((I, A)) => Boolean): P[Boolean] = map(getList)(_.all(p))

  def length: P[Int] = map(getList)(_.length)

  def isEmpty: P[Boolean] = map(getList)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(isEmpty)(! _)
}
