package org.hablapps.phoropter
package core

import Function.const
import scalaz._, Scalaz._

trait Traversal[P[_], Q[_], A] {

  // Generic Van Laarhoven?
  def modifyF[O: Monoid](qo: Q[O]): P[O]

  def getAll(implicit MS: MonadState[Q, A]): P[List[A]] =
    modifyF[List[A]](MS.gets(_.point[List]))

  def set(a: A)(implicit MS: MonadState[Q, A]): P[Unit] =
    modifyF[Unit](MS.put(a))

  def modify(f: A => A)(implicit MS: MonadState[Q, A]): P[Unit] =
    modifyF[Unit](MS.modify(f))

  def count(implicit MS: MonadState[Q, A]): P[Int] =
    modifyF[Int](MS.gets(const(1)))
}

object Traversal {

  // XXX: Is this monoid such a crap as it seems?
  implicit val unitMonoid: Monoid[Unit] =
    Monoid.instance[Unit]((_, _) => (), ())
}
