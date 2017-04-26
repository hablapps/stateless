package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._

import shapeless._, ops.hlist._

trait ITraversalAlg[P[_], I <: HList, A] extends raw.ITraversalAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadState, List] {

  def getList: P[List[(I, A)]] = hom(ev.get.strengthL)

  def modifyList(f: A => A): P[List[Unit]] = hom(_ => ev.modify(f))

  /* composing algebras */

  def composeIFold[J <: HList, K <: HList, B](
      fl: IFoldAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, fl.Q, K, B] =
    asIFold.composeIFold(fl)

  def composeIGetter[J <: HList, K <: HList, B](
      gt: IGetterAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): IFoldAlg.Aux[P, gt.Q, K, B] =
    asIFold.composeIFold(gt.asIFold)

  def composeISetter[J <: HList, K <: HList, B](
      st: ISetterAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ISetterAlg.Aux[P, st.Q, K, B] =
    asISetter.composeISetter(st)

  def composeITraversal[J <: HList, K <: HList, B](
      tr: ITraversalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ITraversalAlg.Aux[P, tr.Q, K, B] =
    ITraversalAlg(λ[λ[x => K => tr.Q[x]] ~> λ[x => P[List[x]]]] { iqx =>
      map(hom(i => tr.hom(j => iqx(i ++ j))))(_.join)
    })(this, tr.ev)

  def composeIOptional[J <: HList, K <: HList, B](
      op: IOptionalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ITraversalAlg.Aux[P, op.Q, K, B] =
    composeITraversal(op.asITraversal)

  def composeILens[J <: HList, K <: HList, B](
      ln: ILensAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ITraversalAlg.Aux[P, ln.Q, K, B] =
    composeITraversal(ln.asITraversal)

  /* transforming algebras */

  def asIFold: IFoldAlg.Aux[P, Q, I, A] = IFoldAlg(hom)(this, ev)

  def asISetter: ISetterAlg.Aux[P, Q, I, A] =
    ISetterAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[Const[Unit, x]]]] { qx =>
      map(hom(qx))(_ => Const(()))
    })(this, ev)
}

object ITraversalAlg {

  type Aux[P[_], Q2[_], I <: HList, A] = ITraversalAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I <: HList, A](
      hom2: λ[x => I => Q2[x]] ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, I, A] = new ITraversalAlg[P, I, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
