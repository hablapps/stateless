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

  /* composing algebras */

  def composeFold[R[_], B](fl: MonadFold[Q, R, B]): MonadFold[P, R, B] =
    MonadFold(λ[R ~> λ[x => P[List[x]]]](rx => hom(fl.hom(rx))))(this, fl.MR)

  def composeGetter[R[_], B](gt: MonadGetter[Q, R, B]): MonadGetter[P, R, B] =
    MonadGetter(hom compose gt.hom)(this, gt.MR)

  def composeTraversal[R[_], B](tr: MonadTraversal[Q, R, B]): MonadFold[P, R, B] =
    MonadFold(λ[R ~> λ[x => P[List[x]]]](rx => hom(tr.hom(rx))))(this, tr.MS)

  def composeOptional[R[_], B](op: MonadOptional[Q, R, B]): MonadFold[P, R, B] =
    MonadFold(λ[R ~> λ[x => P[List[x]]]](rx => hom(op.map(op.hom(rx))(_.toList))))(this, op.MS)

  def composePrism[R[_], B](pr: MonadOptional[Q, R, B]): MonadFold[P, R, B] =
    MonadFold(λ[R ~> λ[x => P[List[x]]]](rx => hom(pr.map(pr.hom(rx))(_.toList))))(this, pr.MS)

  def composeLens[R[_], B](ln: MonadLens[Q, R, B]): MonadGetter[P, R, B] =
    MonadGetter(hom compose ln.hom)(this, ln.MS)
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
