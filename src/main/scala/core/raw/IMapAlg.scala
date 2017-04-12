package org.hablapps.stateless
package core
package raw

import scalaz.Monad
import scalaz.syntax.std.option._

trait IMapAlg[P[_], I, A] extends Monad[P] {

  def getList: P[List[(I, A)]]

  def modifyList(f: A => A): P[List[Unit]]

  def updateOption(i: I)(oa: Option[A]): P[Unit]

  /* derived algebra */

  def modify(f: A => A): P[Unit] = void(modifyList(f))

  def setList(a: A): P[List[Unit]] = modifyList(_ => a)

  def set(a: A): P[Unit] = void(setList(a))

  def indexes: P[List[I]] = map(getList)(_.unzip._1)

  def foci: P[List[A]] = map(getList)(_.unzip._2)

  def add(i: I)(a: A): P[Unit] = updateOption(i)(a.some)

  def remove(i: I): P[Unit] = updateOption(i)(None)

  def get(i: I): P[Option[A]] = map(getList)(_.toMap.get(i))
}
