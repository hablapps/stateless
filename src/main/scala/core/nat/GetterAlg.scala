package org.hablapps.stateless
package core
package nat

import scalaz.{ Functor, Monad, MonadReader, ~> }
import scalaz.Id.Id

import shapeless.HNil

trait GetterAlg[P[_], A] extends OpticAlg[P, A, MonadReader, Id]
    with raw.GetterAlg[P, A] {

  def ask: P[A] = hom[A](ev.ask)

  /* composing algebras */

  def composeFold[B](fl: FoldAlg[Q, B]): FoldAlg.Aux[P, fl.Q, B] =
    asFold.composeFold(fl)

  def composeGetter[B](gt: GetterAlg[Q, B]): GetterAlg.Aux[P, gt.Q, B] =
    GetterAlg(hom compose gt.hom)(this, gt.ev)

  def composeTraversal[B](tr: TraversalAlg[Q, B]): FoldAlg.Aux[P, tr.Q, B] =
    asFold.composeFold(tr.asFold)

  def composeOptional[B](op: OptionalAlg[Q, B]): FoldAlg.Aux[P, op.Q, B] =
    asFold.composeFold(op.asFold)

  def composeLens[B](ln: LensAlg[Q, B]): GetterAlg.Aux[P, ln.Q, B] =
    composeGetter(ln.asGetter)

  /* transforming algebras */

  def asFold: FoldAlg.Aux[P, Q, A] =
    FoldAlg(λ[Q ~> λ[x => P[List[x]]]](qx => map(hom(qx))(List(_))))(this, ev)

  def asIndexed: IGetterAlg.Aux[P, Q, HNil, A] =
    IGetterAlg(new (λ[x => HNil => Q[x]] ~> P) {
      def apply[X](iqx: HNil => Q[X]): P[X] = hom[X](iqx(HNil))
    })(this, ev)

  def asSymmetric: SGetterAlg.Aux[P, Q, Q, A, A] =
    SGetterAlg(hom, hom)(this, ev, ev)

  /* laws */

  trait NatGetterAlgLaw extends GetterAlgLaw with OpticAlgLaw

  def natGetterAlgLaw = new NatGetterAlgLaw {}
}

object GetterAlg {

  type Aux[P[_], Q2[_], A] = GetterAlg[P, A] { type Q[x] = Q2[x] }

  private val fev1 = Functor[Id]

  def apply[P[_], Q2[_], A](
      hom2: Q2 ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q2, A]): Aux[P, Q2, A] = new GetterAlg[P, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    implicit val fev = fev1
    val hom = hom2
  }
}
