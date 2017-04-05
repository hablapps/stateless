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

object MonadPrism {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[Option[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new MonadPrism[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MS = ev1
    val hom = hom2
  }
}
