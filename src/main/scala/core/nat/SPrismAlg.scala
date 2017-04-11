package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }

trait SPrismAlg[P[_], L[_], R[_], A, B] extends raw.SPrismAlg[P, A, B]
    with SOpticAlg[P, L, R, A, B, MonadState, Option] {

  def getOptionL: P[Option[A]] = homL[A](evL.get)

  def setL(a: A): P[Unit] = map(homL(evL.put(a)))(_.get)

  def getOptionR: P[Option[B]] = homR[B](evR.get)

  def setR(b: B): P[Unit] = map(homR(evR.put(b)))(_.get)
}

object SPrismAlg {

  def apply[P[_], L[_], R[_], A, B](
      homL2: L ~> λ[x => P[Option[x]]],
      homR2: R ~> λ[x => P[Option[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[L, A],
      ev2: MonadState[R, B]) = new SPrismAlg[P, L, R, A, B] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val evL = ev1
    implicit val evR = ev2
    val homL = homL2
    val homR = homR2
  }
}
