package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.Id.Id

trait SLensAlg[P[_], A, B] extends raw.SLensAlg[P, A, B]
    with SOpticAlg[P, A, B, MonadState, Id] {

  def getL: P[A] = homL[A](evL.get)

  def setL(a: A): P[Unit] = homL(evL.put(a))

  def getR: P[B] = homR[B](evR.get)

  def setR(b: B): P[Unit] = homR(evR.put(b))
}

object SLensAlg {

  type Aux[P[_], L2[_], R2[_], A, B] = SLensAlg[P, A, B] {
    type L[x] = L2[x]
    type R[x] = R2[x]
  }

  def apply[P[_], L2[_], R2[_], A, B](
      homL2: L2 ~> P,
      homR2: R2 ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadState[L2, A],
      ev2: MonadState[R2, B]): Aux[P, L2, R2, A, B] = new SLensAlg[P, A, B] {
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
