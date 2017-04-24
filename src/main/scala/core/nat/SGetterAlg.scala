package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.Id.Id

trait SGetterAlg[P[_], A, B] extends raw.SGetterAlg[P, A, B]
    with SOpticAlg[P, A, B, MonadReader, Id] {

  def getL: P[A] = homL[A](evL.ask)

  def getR: P[B] = homR[B](evR.ask)
}

object SGetterAlg {

  type Aux[P[_], L2[_], R2[_], A, B] = SGetterAlg[P, A, B] {
    type L[x] = L2[x]
    type R[x] = R2[x]
  }

  def apply[P[_], L2[_], R2[_], A, B](
      homL2: L2 ~> P,
      homR2: R2 ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadReader[L2, A],
      ev2: MonadReader[R2, B]): Aux[P, L2, R2, A, B] = new SGetterAlg[P, A, B] {
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
