package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Monad, MonadState, ~> }

trait ISetterAlg[P[_], Q[_], I, A] extends raw.ISetterAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadState, Const[Unit, ?]] {

  def modify(f: A => A): P[Unit] = map(hom(_ => ev.modify(f)))(_.getConst)
}

object ISetterAlg {

  def apply[P[_], Q[_], I, A](
      hom2: λ[x => I => Q[x]] ~> λ[x => P[Const[Unit, x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new ISetterAlg[P, Q, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
