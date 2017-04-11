package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.Id.Id

trait GetterAlg[P[_], Q[_], A] extends OpticAlg[P, Q, A, MonadReader, Id]
    with raw.GetterAlg[P, A] {

  def ask: P[A] = hom[A](ev.ask)

  /* composing algebras */

  def composeFold[R[_], B](fl: FoldAlg[Q, R, B]): FoldAlg[P, R, B] =
    asFold.composeFold(fl)

  def composeGetter[R[_], B](gt: GetterAlg[Q, R, B]): GetterAlg[P, R, B] =
    GetterAlg(hom compose gt.hom)(this, gt.ev)

  def composeTraversal[R[_], B](tr: TraversalAlg[Q, R, B]): FoldAlg[P, R, B] =
    asFold.composeFold(tr.asFold)

  def composeOptional[R[_], B](op: OptionalAlg[Q, R, B]): FoldAlg[P, R, B] =
    asFold.composeFold(op.asFold)

  def composePrism[R[_], B](pr: PrismAlg[Q, R, B]): FoldAlg[P, R, B] =
    asFold.composeFold(pr.asFold)

  def composeLens[R[_], B](ln: LensAlg[Q, R, B]): GetterAlg[P, R, B] =
    composeGetter(ln.asGetter)

  /* transforming algebras */

  def asFold: FoldAlg[P, Q, A] =
    FoldAlg(λ[Q ~> λ[x => P[List[x]]]](qx => map(hom(qx))(List(_))))(this, ev)

  def asIndexed: IGetterAlg[P, Q, Unit, A] =
    IGetterAlg(new (λ[x => Unit => Q[x]] ~> P) {
      def apply[X](iqx: Unit => Q[X]): P[X] = hom[X](iqx(()))
    })(this, ev)

  def asSymmetric: SGetterAlg[P, Q, Q, A, A] =
    SGetterAlg(hom, hom)(this, ev, ev)
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
