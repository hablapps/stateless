package org.hablapps.phoropter
package core

import scalaz._, Scalaz._

trait Lens[P[_], Q[_], A] {

  // Generic Van Laarhoven?
  def modifyF[O](qo: Q[O]): P[O]

  def get(implicit MS: MonadState[Q, A]): P[A] =
    modifyF[A](MS.get)

  def set(a: A)(implicit MS: MonadState[Q, A]): P[Unit] =
    modifyF[Unit](MS.put(a))

  def modify(f: A => A)(implicit MS: MonadState[Q, A]): P[Unit] =
    modifyF[Unit](MS.modify(f))
}
