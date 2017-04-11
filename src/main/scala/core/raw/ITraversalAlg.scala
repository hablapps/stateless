package org.hablapps.stateless
package core
package raw

import scalaz.Monad

trait ITraversalAlg[P[_], I, A] extends Monad[P] {

  def getList: P[List[(I, A)]]

  def modifyList(f: A => A): P[List[Unit]]

  def modify(f: A => A): P[Unit] = map(modifyList(f))(_ => ())

  def setList(a: A): P[List[Unit]] = modifyList(_ => a)

  def set(a: A): P[Unit] = map(setList(a))(_ => ())

  def indexes: P[List[I]] = map(getList)(_.unzip._1)

  def foci: P[List[A]] = map(getList)(_.unzip._2)
}
