package org.hablapps.phoropter
package core

import scalaz.{ Const, Monad, MonadState, ~> }

trait MonadSetter[P[_], Q[_], A] extends Monad[P] {

  implicit val MS: MonadState[Q, A] // should be restricted to `MonadPut`
  val hom: Q ~> λ[x => P[Const[Unit, x]]]

  /* derived algebra */

  def modify(f: A => A): P[Unit] = map(hom(MS.modify(f)))(_.getConst)

  def set(a: A): P[Unit] = modify(_ => a)
}

object MonadSetter {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[Const[Unit, x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new MonadSetter[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MS = ev1
    val hom = hom2
  }
}
