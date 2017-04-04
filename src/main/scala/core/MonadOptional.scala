package org.hablapps.phoropter
package core

import scalaz.{ Monad, MonadState, ~> }

trait MonadOptional[P[_], Q[_], A] extends Monad[P] {

  implicit val MS: MonadState[Q, A]
  val hom: Q ~> λ[x => P[Option[x]]]

  /* derived algebra */

  def getOpt: P[Option[A]] = hom(MS.get)
  def set(a: A): P[Option[Unit]] = hom(MS.put(a))
}
