package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.option._

trait IOptionalAlg[P[_], I, A] extends raw.IOptionalAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadState, Option] {

  def getOption: P[Option[(I, A)]] = hom(ev.get.strengthL)

  def setOption(a: A): P[Option[Unit]] = hom(_ => ev.put(a))

  /* composing algebras */

  def composeIFold[J, B](fl: IFoldAlg[Q, J, B]): IFoldAlg.Aux[P, fl.Q, (I, J), B] =
    asIFold.composeIFold(fl)

  def composeIGetter[J, B](gt: IGetterAlg[Q, J, B]): IFoldAlg.Aux[P, gt.Q, (I, J), B] =
    asIFold.composeIFold(gt.asIFold)

  def composeISetter[J, B](st: ISetterAlg[Q, J, B]): ISetterAlg.Aux[P, st.Q, (I, J), B] =
    asISetter.composeISetter(st)

  def composeITraversal[J, B](tr: ITraversalAlg[Q, J, B]): ITraversalAlg.Aux[P, tr.Q, (I, J), B] =
    asITraversal.composeITraversal(tr)

  def composeIOptional[J, B](op: IOptionalAlg[Q, J, B]): IOptionalAlg.Aux[P, op.Q, (I, J), B] =
    IOptionalAlg(λ[λ[x => ((I, J)) => op.Q[x]] ~> λ[x => P[Option[x]]]] { iqx =>
      map(hom(i => op.hom(j => iqx((i, j)))))(_.join)
    })(this, op.ev)

  def composeILens[J, B](ln: ILensAlg[Q, J, B]): IOptionalAlg.Aux[P, ln.Q, (I, J), B] =
    composeIOptional(ln.asIOptional)

  /* transforming algebras */

  def asITraversal: ITraversalAlg.Aux[P, Q, I, A] =
    ITraversalAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[List[x]]]] { qx =>
      map(hom(qx))(_.toList)
    })(this, ev)

  def asISetter: ISetterAlg.Aux[P, Q, I, A] = asITraversal.asISetter

  def asIFold: IFoldAlg.Aux[P, Q, I, A] = asITraversal.asIFold
}

object IOptionalAlg {

  type Aux[P[_], Q2[_], I, A] = IOptionalAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I, A](
      hom2: λ[x => I => Q2[x]] ~> λ[y => P[Option[y]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, I, A] = new IOptionalAlg[P, I, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
