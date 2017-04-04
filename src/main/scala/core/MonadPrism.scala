package org.hablapps.phoropter
package core

import scalaz.{ Monad, MonadState, ~> }

trait MonadPrism[P[_], Q[_], A] extends Monad[P] {

  implicit val MS: MonadState[Q, A]
  val hom: Q ~> λ[x => P[Option[x]]]

  /* derived algebra */

  def getOpt: P[Option[A]] = hom(MS.get)
  def set(a: A): P[Unit] = map(hom(MS.put(a)))(_.get) // safe by laws

  def modify(f: A => A): P[Unit] =
    bind(getOpt)(_.fold(point(()))(a => set(f(a))))
}
