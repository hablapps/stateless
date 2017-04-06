package org.hablapps.phoropter
package core

import scalaz.{ Const, Monad, MonadState, ~> }

trait MonadLens[P[_], Q[_], A] extends MonadState[P, A] {

  implicit val MS: MonadState[Q, A]
  val hom: Q ~> P

  /* derived algebra */

  override def get: P[A] = hom(MS.get)

  def set(a: A): P[Unit] = hom(MS.put(a))

  override def put(a: A): P[Unit] = set(a)

  override def init: P[A] = hom(MS.get)

  /* composing algebras */

  def composeFold[R[_], B](fl: MonadFold[Q, R, B]): MonadFold[P, R, B] =
    MonadFold(λ[R ~> λ[x => P[List[x]]]](rx => hom(fl.hom(rx))))(this, fl.MR)

  def composeGetter[R[_], B](gt: MonadGetter[Q, R, B]): MonadGetter[P, R, B] =
    MonadGetter(hom compose gt.hom)(this, gt.MR)

  def composeSetter[R[_], B](st: MonadSetter[Q, R, B]): MonadSetter[P, R, B] =
    MonadSetter(λ[R ~> λ[x => P[Const[Unit, x]]]](rx => hom(st.hom(rx))))(this, st.MS)

  def composeTraversal[R[_], B](tr: MonadTraversal[Q, R, B]): MonadTraversal[P, R, B] =
    MonadTraversal(λ[R ~> λ[x => P[List[x]]]](rx => hom(tr.hom(rx))))(this, tr.MS)

  def composeOptional[R[_], B](op: MonadOptional[Q, R, B]): MonadOptional[P, R, B] =
    MonadOptional(λ[R ~> λ[x => P[Option[x]]]](rx => hom(op.hom(rx))))(this, op.MS)

  def composePrism[R[_], B](pr: MonadPrism[Q, R, B]): MonadOptional[P, R, B] =
    MonadOptional(λ[R ~> λ[x => P[Option[x]]]](rx => hom(pr.hom(rx))))(this, pr.MS)

  def composeLens[R[_], B](ln: MonadLens[Q, R, B]): MonadLens[P, R, B] =
    MonadLens(hom compose ln.hom)(this, ln.MS)
}

object MonadLens {

  def apply[P[_], Q[_], A](
      hom2: Q ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new MonadLens[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MS = ev1
    val hom = hom2
  }
}
