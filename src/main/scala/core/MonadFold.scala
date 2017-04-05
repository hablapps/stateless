package org.hablapps.phoropter
package core

import scalaz.{ Monad, MonadReader, ~> }

trait MonadFold[P[_], Q[_], A] extends Monad[P] {

  implicit val MR: MonadReader[Q, A]
  val hom: Q ~> λ[x => P[List[x]]]

  /* derived algebra */

  def getAll: P[List[A]] = hom(MR.ask)
}

object MonadFold {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q, A]) = new MonadFold[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MR = ev1
    val hom = hom2
  }
}
