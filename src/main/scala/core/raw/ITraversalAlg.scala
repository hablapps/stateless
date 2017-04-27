package org.hablapps.stateless
package core
package raw

import scalaz.{ Monad, Monoid }
import scalaz.syntax.foldable._
import scalaz.std.list._

trait ITraversalAlg[P[_], I, A] extends Monad[P] {

  def getList: P[List[(I, A)]]

  def modifyList(f: A => A): P[List[Unit]]

  /* derived methods */

  def foldMap[M: Monoid](f: A => M): P[M] = map(foci)(_.foldMap(f))

  def modify(f: A => A): P[Unit] = void(modifyList(f))

  def setList(a: A): P[List[Unit]] = modifyList(_ => a)

  def set(a: A): P[Unit] = void(setList(a))

  def indexes: P[List[I]] = map(getList)(_.unzip._1)

  def foci: P[List[A]] = map(getList)(_.unzip._2)
}
