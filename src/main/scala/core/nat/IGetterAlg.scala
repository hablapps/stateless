package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.Id.Id
import scalaz.syntax.functor._

import shapeless._, ops.hlist._

trait IGetterAlg[P[_], I <: HList, A] extends raw.IGetterAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadReader, Id] {

  def get: P[(I, A)] = hom(ev.ask.strengthL)

  /* composing algebras */

  def composeIFold[J <: HList, K <: HList, B](
      fl: IFoldAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, fl.Q, K, B] =
    asIFold.composeIFold(fl)

  def composeIGetter[J <: HList, K <: HList, B](
      gt: IGetterAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IGetterAlg.Aux[P, gt.Q, K, B] =
    IGetterAlg(new (λ[x => K => gt.Q[x]] ~> P) {
      def apply[X](iqx: K => gt.Q[X]): P[X] =
        hom[X](i => gt.hom[X](j => iqx(i ++ j)))
    })(this, gt.ev)

  def composeITraversal[J <: HList, K <: HList, B](
      tr: ITraversalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, tr.Q, K, B] =
    composeIFold(tr.asIFold)

  def composeIOptional[J <: HList, K <: HList, B](
      op: IOptionalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, op.Q, K, B] =
    composeIFold(op.asIFold)

  def composeILens[J <: HList, K <: HList, B](
      ln: ILensAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IGetterAlg.Aux[P, ln.Q, K, B] =
    composeIGetter(ln.asIGetter)

  /* transforming algebras */

  def asIFold: IFoldAlg.Aux[P, Q, I, A] =
    IFoldAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[List[x]]]] { qx =>
      map(hom(qx))(List(_))
    })(this, ev)
}

object IGetterAlg {

  type Aux[P[_], Q2[_], I <: HList, A] = IGetterAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I <: HList, A](
      hom2: λ[x => I => Q2[x]] ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q2, A]): Aux[P, Q2, I, A] = new IGetterAlg[P, I, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
