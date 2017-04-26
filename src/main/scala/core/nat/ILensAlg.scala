package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.Id.Id
import scalaz.syntax.functor._
import scalaz.syntax.std.option._

import shapeless._, ops.hlist._

trait ILensAlg[P[_], I <: HList, A] extends raw.ILensAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadState, Id] {

  def get: P[(I, A)] = hom(ev.get.strengthL)

  def set(a: A): P[Unit] = hom(_ => ev.put(a))

  /* composing algebras */

  def composeIFold[J <: HList, K <: HList, B](
      fl: IFoldAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, fl.Q, K, B] =
    asIFold.composeIFold(fl)

  def composeIGetter[J <: HList, K <: HList, B](
      gt: IGetterAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IGetterAlg.Aux[P, gt.Q, K, B] =
    asIGetter.composeIGetter(gt)

  def composeISetter[J <: HList, K <: HList, B](
      st: ISetterAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ISetterAlg.Aux[P, st.Q, K, B] =
    asISetter.composeISetter(st)

  def composeITraversal[J <: HList, K <: HList, B](
      tr: ITraversalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ITraversalAlg.Aux[P, tr.Q, K, B] =
    asITraversal.composeITraversal(tr)

  def composeIOptional[J <: HList, K <: HList, B](
      op: IOptionalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IOptionalAlg.Aux[P, op.Q, K, B] =
    asIOptional.composeIOptional(op)

  def composeILens[J <: HList, K <: HList, B](
      ln: ILensAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ILensAlg.Aux[P, ln.Q, K, B] =
    ILensAlg(new (λ[x => K => ln.Q[x]] ~> P) {
      def apply[X](iqx: K => ln.Q[X]): P[X] =
        hom[X](i => ln.hom[X](j => iqx(i ++ j)))
    })(this, ln.ev)

  /* transforming algebras */

  def asIGetter: IGetterAlg.Aux[P, Q, I, A] = IGetterAlg(hom)(this, ev)

  def asIOptional: IOptionalAlg.Aux[P, Q, I, A] =
    IOptionalAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[Option[x]]]](
      qx => map(hom(qx))(_.some)))(this, ev)

  def asIFold: IFoldAlg.Aux[P, Q, I, A] = asIGetter.asIFold

  def asITraversal: ITraversalAlg.Aux[P, Q, I, A] = asIOptional.asITraversal

  def asISetter: ISetterAlg.Aux[P, Q, I, A] = asITraversal.asISetter
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
}
