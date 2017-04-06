package org.hablapps.phoropter
package core

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.Id.Id

trait LensAlg[P[_], Q[_], A] extends OpticAlg[P, Q, A, MonadState, Id]
    with MonadState[P, A] {

  override def get: P[A] = hom[A](ev.get)

  def set(a: A): P[Unit] = hom(ev.put(a))

  override def put(a: A): P[Unit] = set(a)

  override def init: P[A] = hom[A](ev.get)

  /* composing algebras */

  def composeFold[R[_], B](fl: FoldAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]](rx => hom(fl.hom(rx))))(this, fl.ev)

  def composeGetter[R[_], B](gt: GetterAlg[Q, R, B]): GetterAlg[P, R, B] =
    GetterAlg(hom compose gt.hom)(this, gt.ev)

  def composeSetter[R[_], B](st: SetterAlg[Q, R, B]): SetterAlg[P, R, B] =
    SetterAlg(λ[R ~> λ[x => P[Const[Unit, x]]]](rx => hom(st.hom(rx))))(this, st.ev)

  def composeTraversal[R[_], B](tr: TraversalAlg[Q, R, B]): TraversalAlg[P, R, B] =
    TraversalAlg(λ[R ~> λ[x => P[List[x]]]](rx => hom(tr.hom(rx))))(this, tr.ev)

  def composeOptional[R[_], B](op: OptionalAlg[Q, R, B]): OptionalAlg[P, R, B] =
    OptionalAlg(λ[R ~> λ[x => P[Option[x]]]](rx => hom(op.hom(rx))))(this, op.ev)

  def composePrism[R[_], B](pr: PrismAlg[Q, R, B]): OptionalAlg[P, R, B] =
    OptionalAlg(λ[R ~> λ[x => P[Option[x]]]](rx => hom(pr.hom(rx))))(this, pr.ev)

  def composeLens[R[_], B](ln: LensAlg[Q, R, B]): LensAlg[P, R, B] =
    LensAlg(hom compose ln.hom)(this, ln.ev)
}

object LensAlg {

  def apply[P[_], Q[_], A](
      hom2: Q ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new LensAlg[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
