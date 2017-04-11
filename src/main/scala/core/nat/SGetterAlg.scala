package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.Id.Id

trait SGetterAlg[P[_], L[_], R[_], A, B] extends raw.SGetterAlg[P, A, B]
    with SOpticAlg[P, L, R, A, B, MonadReader, Id] {

  def getL: P[A] = homL[A](evL.ask)

  def getR: P[B] = homR[B](evR.ask)
}

object SGetterAlg {

  def apply[P[_], L[_], R[_], A, B](
      homL2: L ~> P,
      homR2: R ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadReader[L, A],
      ev2: MonadReader[R, B]) = new SGetterAlg[P, L, R, A, B] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val evL = ev1
    implicit val evR = ev2
    val homL = homL2
    val homR = homR2
  }
}
