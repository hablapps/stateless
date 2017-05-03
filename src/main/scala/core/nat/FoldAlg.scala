package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._

import shapeless._

trait FoldAlg[P[_], A] extends OpticAlg[P, A, MonadReader, List]
    with raw.FoldAlg[P, A] {

  def getList: P[List[A]] = hom(ev.ask)

  /* composing algebras */

  def composeFold[B](fl: FoldAlg[Q, B]): FoldAlg.Aux[P, fl.Q, B] =
    FoldAlg(λ[fl.Q ~> λ[x => P[List[x]]]](rx => map(hom(fl.hom(rx)))(_.join)))(this, fl.ev)

  def composeGetter[B](gt: GetterAlg[Q, B]): FoldAlg.Aux[P, gt.Q, B] =
    composeFold(gt.asFold)

  def composeTraversal[B](tr: TraversalAlg[Q, B]): FoldAlg.Aux[P, tr.Q, B] =
    composeFold(tr.asFold)

  def composeOptional[B](op: OptionalAlg[Q, B]): FoldAlg.Aux[P, op.Q, B] =
    composeFold(op.asFold)

  def composeLens[B](ln: LensAlg[Q, B]): FoldAlg.Aux[P, ln.Q, B] =
    composeFold(ln.asFold)

  def composeFold[I <: HList, B](fl: IFoldAlg[Q, I, B]): IFoldAlg.Aux[P, fl.Q, I, B] =
    asIndexed.composeFold(fl)

  /* transforming algebras */

  def asIndexed: IFoldAlg.Aux[P, Q, HNil, A] =
    IFoldAlg(λ[λ[x => HNil => Q[x]] ~> λ[x => P[List[x]]]] { iqx =>
      hom(iqx(HNil))
    })(this, ev)

  def asSymmetric: SFoldAlg.Aux[P, Q, Q, A, A] =
    SFoldAlg(hom, hom)(this, ev, ev)
}

object FoldAlg {

  type Aux[P[_], Q2[_], A] = FoldAlg[P, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], A](
      hom2: Q2 ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q2, A]): Aux[P, Q2, A] = new FoldAlg[P, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
