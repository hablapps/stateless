package org.hablapps.phoropter
package core

import scalaz.{ Monad, MonadReader, ~> }

trait MonadGetter[P[_], Q[_], A] extends MonadReader[P, A] {

  implicit val MR: MonadReader[Q, A]
  val hom: Q ~> P // monad homomorphism

  /* derived algebra */

  def get: P[A] = hom(MR.ask)

  override def ask = get

  // FIXME: dummy implementation
  override def local[X](f: A => A)(px: P[X]) = px
}

object MonadGetter {

  def apply[P[_], Q[_], A](
      hom2: Q ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q, A]) = new MonadGetter[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MR = ev1
    val hom = hom2
  }
}
