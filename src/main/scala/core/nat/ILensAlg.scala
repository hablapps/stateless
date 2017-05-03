package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.Id.Id
import scalaz.Leibniz.===
import scalaz.syntax.functor._
import scalaz.syntax.std.option._

import shapeless._, ops.hlist._

trait ILensAlg[P[_], I <: HList, A] extends raw.ILensAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadState, Id] {

  def get: P[(I, A)] = hom(ev.get.strengthL)

  def set(a: A): P[Unit] = hom(_ => ev.put(a))

  /* composing algebras */

  def composeFold[J <: HList, K <: HList, B](
      fl: IFoldAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, fl.Q, K, B] =
    asFold.composeFold(fl)

  def composeGetter[J <: HList, K <: HList, B](
      gt: IGetterAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IGetterAlg.Aux[P, gt.Q, K, B] =
    asGetter.composeGetter(gt)

  def composeSetter[J <: HList, K <: HList, B](
      st: ISetterAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ISetterAlg.Aux[P, st.Q, K, B] =
    asSetter.composeSetter(st)

  def composeTraversal[J <: HList, K <: HList, B](
      tr: ITraversalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ITraversalAlg.Aux[P, tr.Q, K, B] =
    asTraversal.composeTraversal(tr)

  def composeOptional[J <: HList, K <: HList, B](
      op: IOptionalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IOptionalAlg.Aux[P, op.Q, K, B] =
    asOptional.composeOptional(op)

  def composeLens[J <: HList, K <: HList, B](
      ln: ILensAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ILensAlg.Aux[P, ln.Q, K, B] =
    ILensAlg(new (λ[x => K => ln.Q[x]] ~> P) {
      def apply[X](iqx: K => ln.Q[X]): P[X] =
        hom[X](i => ln.hom[X](j => iqx(i ++ j)))
    })(this, ln.ev)

  /* transforming algebras */

  def asGetter: IGetterAlg.Aux[P, Q, I, A] = IGetterAlg(hom)(this, ev)

  def asOptional: IOptionalAlg.Aux[P, Q, I, A] =
    IOptionalAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[Option[x]]]](
      qx => map(hom(qx))(_.some)))(this, ev)

  def asFold: IFoldAlg.Aux[P, Q, I, A] = asGetter.asFold

  def asTraversal: ITraversalAlg.Aux[P, Q, I, A] = asOptional.asTraversal

  def asSetter: ISetterAlg.Aux[P, Q, I, A] = asTraversal.asSetter

  def asPlain(implicit ev0: I === HNil): LensAlg.Aux[P, Q, A] =
    LensAlg[P, Q, A](new (Q ~> P) {
      def apply[X](qx: Q[X]): P[X] = hom[X](_ => qx)
    })(this, ev)
}

object ILensAlg {

  type Aux[P[_], Q2[_], I <: HList, A] = ILensAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I <: HList, A](
      hom2: λ[x => I => Q2[x]] ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, I, A] = new ILensAlg[P, I, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }

  implicit def toIndexed[P[_], A](ln: LensAlg[P, A]): Aux[P, ln.Q, HNil, A] =
    ln.asIndexed
}
