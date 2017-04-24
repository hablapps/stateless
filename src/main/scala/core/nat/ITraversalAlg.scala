package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._

trait ITraversalAlg[P[_], I, A] extends raw.ITraversalAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadState, List] {

  def getList: P[List[(I, A)]] = hom(ev.get.strengthL)

  def modifyList(f: A => A): P[List[Unit]] = hom(_ => ev.modify(f))

  /* composing algebras */
  
  def composeIFold[J, B](fl: IFoldAlg[Q, J, B]): IFoldAlg.Aux[P, fl.Q, (I, J), B] =
    asIFold.composeIFold(fl)

  def composeIGetter[J, B](gt: IGetterAlg[Q, J, B]): IFoldAlg.Aux[P, gt.Q, (I, J), B] =
    asIFold.composeIFold(gt.asIFold)

  def composeISetter[J, B](st: ISetterAlg[Q, J, B]): ISetterAlg.Aux[P, st.Q, (I, J), B] =
    asISetter.composeISetter(st)

  def composeITraversal[J, B](tr: ITraversalAlg[Q, J, B]): ITraversalAlg.Aux[P, tr.Q, (I, J), B] =
    ITraversalAlg(λ[λ[x => ((I, J)) => tr.Q[x]] ~> λ[x => P[List[x]]]] { iqx =>
      map(hom(i => tr.hom(j => iqx((i, j)))))(_.join)
    })(this, tr.ev)

  def composeIOptional[J, B](op: IOptionalAlg[Q, J, B]): ITraversalAlg.Aux[P, op.Q, (I, J), B] =
    composeITraversal(op.asITraversal)

  def composeILens[J, B](ln: ILensAlg[Q, J, B]): ITraversalAlg.Aux[P, ln.Q, (I, J), B] =
    composeITraversal(ln.asITraversal)

  /* transforming algebras */

  def asIFold: IFoldAlg.Aux[P, Q, I, A] = IFoldAlg(hom)(this, ev)

  def asISetter: ISetterAlg.Aux[P, Q, I, A] =
    ISetterAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[Const[Unit, x]]]] { qx =>
      map(hom(qx))(_ => Const(()))
    })(this, ev)
}

object ITraversalAlg {

  type Aux[P[_], Q2[_], I, A] = ITraversalAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I, A](
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
