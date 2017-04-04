package org.hablapps.phoropter
package core

import scalaz.{ Monad, MonadState, ~> }

trait MonadSetter[P[_], Q[_], A] extends Monad[P] {

  implicit val MS: MonadState[Q, A]
  val hom: Q ~> P

  /* derived algebra */

  def modify(f: A => A): P[Unit] = hom(MS.modify(f))
  def set(a: A): P[Unit] = modify(_ => a)
}
