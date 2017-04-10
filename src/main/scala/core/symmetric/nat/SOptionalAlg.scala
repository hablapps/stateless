package org.hablapps.phoropter
package core
package symmetric
package nat

import scalaz.{ Monad, MonadState, ~> }

trait SOptionalAlg[P[_], L[_], R[_], A, B] extends raw.SOptionalAlg[P, A, B]
    with SOpticAlg[P, L, R, A, B, MonadState, Option] {

  def getOptionL: P[Option[A]] = homL[A](evL.get)

  def setOptionL(a: A): P[Option[Unit]] = homL(evL.put(a))

  def getOptionR: P[Option[B]] = homR[B](evR.get)

  def setOptionR(b: B): P[Option[Unit]] = homR(evR.put(b))
}

object SOptionalAlg {

  def apply[P[_], L[_], R[_], A, B](
      homL2: L ~> λ[x => P[Option[x]]],
      homR2: R ~> λ[x => P[Option[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[L, A],
      ev2: MonadState[R, B]) = new SOptionalAlg[P, L, R, A, B] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val evL = ev1
    implicit val evR = ev2
    val homL = homL2
    val homR = homR2
  }
}
