package org.hablapps.phoropter
package core

import Function.const
import scalaz._, Scalaz._

trait Traversal[P[_], Q[_], A] {

  implicit val MS: MonadState[Q, A]

  def modifyF[O: Monoid](qo: Q[O]): P[O]

  // derived methods

  def getAll: P[List[A]] = modifyF[List[A]](MS.gets(_.point[List]))

  def set(a: A): P[Unit] = modifyF[Unit](MS.put(a))

  def modify(f: A => A): P[Unit] = modifyF[Unit](MS.modify(f))

  def count: P[Int] = modifyF[Int](MS.gets(const(1)))
}

object Traversal {

  // XXX: Is this monoid such a crap as it seems?
  implicit val unitMonoid: Monoid[Unit] =
    Monoid.instance[Unit]((_, _) => (), ())
}
