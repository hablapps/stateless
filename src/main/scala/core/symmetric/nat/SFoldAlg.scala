package org.hablapps.stateless
package core
package symmetric
package nat

import scalaz.{ Monad, MonadReader, ~> }

trait SFoldAlg[P[_], L[_], R[_], A, B] extends raw.SFoldAlg[P, A, B]
    with SOpticAlg[P, L, R, A, B, MonadReader, List] {

  def getListL: P[List[A]] = homL[A](evL.ask)

  def getListR: P[List[B]] = homR[B](evR.ask)
}

object SFoldAlg {

  def apply[P[_], L[_], R[_], A, B](
      homL2: L ~> λ[x => P[List[x]]],
      homR2: R ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadReader[L, A],
      ev2: MonadReader[R, B]) = new SFoldAlg[P, L, R, A, B] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val evL = ev1
    implicit val evR = ev2
    val homL = homL2
    val homR = homR2
  }
}
