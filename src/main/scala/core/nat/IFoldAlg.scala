package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.std.list._
import scalaz.syntax.monad._

import shapeless._, ops.hlist._

trait IFoldAlg[P[_], I <: HList, A] extends raw.IFoldAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadReader, List] {

  def getList: P[List[(I, A)]] = hom(ev.ask.strengthL)

  /* composing algebras */

  def composeIFold[J <: HList, K <: HList, B](
      fl: IFoldAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, fl.Q, K, B] =
    IFoldAlg(λ[λ[x => K => fl.Q[x]] ~> λ[x => P[List[x]]]] { iqx =>
      map(hom(i => fl.hom(j => iqx(i ++ j))))(_.join)
    })(this, fl.ev)

  def composeIGetter[J <: HList, K <: HList, B](
      gt: IGetterAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, gt.Q, K, B] =
    composeIFold(gt.asIFold)

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
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, ln.Q, K, B] =
    composeIFold(ln.asIFold)
}

object IFoldAlg {

  type Aux[P[_], Q2[_], I <: HList, A] = IFoldAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I <: HList, A](
      hom2: λ[x => I => Q2[x]] ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q2, A]): Aux[P, Q2, I, A] = new IFoldAlg[P, I, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
