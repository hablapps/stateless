package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Monad, MonadState, ~> }

trait SSetterAlg[P[_], A, B] extends raw.SSetterAlg[P, A, B]
    with SOpticAlg[P, A, B, MonadState, Const[Unit, ?]] {

  def setL(a: A): P[Unit] = map(homL(evL.put(a)))(_.getConst)

  def setR(b: B): P[Unit] = map(homR(evR.put(b)))(_.getConst)
}

object SSetterAlg {

  type Aux[P[_], L2[_], R2[_], A, B] = SSetterAlg[P, A, B] {
    type L[x] = L2[x]
    type R[x] = R2[x]
  }

  def apply[P[_], L2[_], R2[_], A, B](
      homL2: L2 ~> λ[x => P[Const[Unit, x]]],
      homR2: R2 ~> λ[x => P[Const[Unit, x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[L2, A],
      ev2: MonadState[R2, B]): Aux[P, L2, R2, A, B] = new SSetterAlg[P, A, B] {
    type L[x] = L2[x]
    type R[x] = R2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val evL = ev1
    implicit val evR = ev2
    val homL = homL2
    val homR = homR2
  }
}
