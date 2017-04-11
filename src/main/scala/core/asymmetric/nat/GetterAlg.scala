package org.hablapps.stateless
package core
package asymmetric
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.Id.Id

trait GetterAlg[P[_], Q[_], A] extends OpticAlg[P, Q, A, MonadReader, Id]
    with raw.GetterAlg[P, A] {

  def ask: P[A] = hom[A](ev.ask)

  /* composing algebras */

  def composeFold[R[_], B](fl: FoldAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]](rx => hom(fl.hom(rx))))(this, fl.ev)

  def composeGetter[R[_], B](gt: GetterAlg[Q, R, B]): GetterAlg[P, R, B] =
    GetterAlg(hom compose gt.hom)(this, gt.ev)

  def composeTraversal[R[_], B](tr: TraversalAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]](rx => hom(tr.hom(rx))))(this, tr.ev)

  def composeOptional[R[_], B](op: OptionalAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]](rx => hom(op.map(op.hom(rx))(_.toList))))(this, op.ev)

  def composePrism[R[_], B](op: OptionalAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]](rx => hom(op.map(op.hom(rx))(_.toList))))(this, op.ev)

  def composeLens[R[_], B](ln: LensAlg[Q, R, B]): GetterAlg[P, R, B] =
    GetterAlg(hom compose ln.hom)(this, ln.ev)

  /* transforming algebras */

  def asFold: FoldAlg[P, Q, A] =
    FoldAlg(λ[Q ~> λ[x => P[List[x]]]](qx => map(hom(qx))(List(_))))(this, ev)
}

object GetterAlg {

  def apply[P[_], Q[_], A](
      hom2: Q ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q, A]) = new GetterAlg[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
