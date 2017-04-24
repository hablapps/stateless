package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }

trait SOptionalAlg[P[_], A, B] extends raw.SOptionalAlg[P, A, B]
    with SOpticAlg[P, A, B, MonadState, Option] {

  def getOptionL: P[Option[A]] = homL[A](evL.get)

  def setOptionL(a: A): P[Option[Unit]] = homL(evL.put(a))

  def getOptionR: P[Option[B]] = homR[B](evR.get)

  def setOptionR(b: B): P[Option[Unit]] = homR(evR.put(b))
}

object SOptionalAlg {

  type Aux[P[_], L2[_], R2[_], A, B] = SOptionalAlg[P, A, B] {
    type L[x] = L2[x]
    type R[x] = R2[x]
  }

  def apply[P[_], L2[_], R2[_], A, B](
      homL2: L2 ~> λ[x => P[Option[x]]],
      homR2: R2 ~> λ[x => P[Option[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[L2, A],
      ev2: MonadState[R2, B]): Aux[P, L2, R2, A, B] = new SOptionalAlg[P, A, B] {
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
