package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }

trait STraversalAlg[P[_], A, B] extends raw.STraversalAlg[P, A, B]
    with SOpticAlg[P, A, B, MonadState, List] {

  def getListL: P[List[A]] = homL[A](evL.get)

  def modifyListL(f: A => A): P[List[Unit]] = homL(evL.modify(f))

  def getListR: P[List[B]] = homR[B](evR.get)

  def modifyListR(f: B => B): P[List[Unit]] = homR(evR.modify(f))
}

object STraversalAlg {

  type Aux[P[_], L2[_], R2[_], A, B] = STraversalAlg[P, A, B] {
    type L[x] = L2[x]
    type R[x] = R2[x]
  }

  def apply[P[_], L2[_], R2[_], A, B](
      homL2: L2 ~> λ[x => P[List[x]]],
      homR2: R2 ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[L2, A],
      ev2: MonadState[R2, B]): Aux[P, L2, R2, A, B] = new STraversalAlg[P, A, B] {
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
