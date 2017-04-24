package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.std.list._
import scalaz.syntax.monad._

trait IFoldAlg[P[_], I, A] extends raw.IFoldAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadReader, List] {

  def getList: P[List[(I, A)]] = hom(ev.ask.strengthL)

  /* composing algebras */

  def composeIFold[J, B](fl: IFoldAlg[Q, J, B]): IFoldAlg.Aux[P, fl.Q, (I, J), B] =
    IFoldAlg(λ[λ[x => ((I, J)) => fl.Q[x]] ~> λ[x => P[List[x]]]] { iqx =>
      map(hom(i => fl.hom(j => iqx((i, j)))))(_.join)
    })(this, fl.ev)

  def composeIGetter[J, B](gt: IGetterAlg[Q, J, B]): IFoldAlg.Aux[P, gt.Q, (I, J), B] =
    composeIFold(gt.asIFold)

  def composeITraversal[J, B](tr: ITraversalAlg[Q, J, B]): IFoldAlg.Aux[P, tr.Q, (I, J), B] =
    composeIFold(tr.asIFold)

  def composeIOptional[J, B](op: IOptionalAlg[Q, J, B]): IFoldAlg.Aux[P, op.Q, (I, J), B] =
    composeIFold(op.asIFold)

  def composeILens[J, B](ln: ILensAlg[Q, J, B]): IFoldAlg.Aux[P, ln.Q, (I, J), B] =
    composeIFold(ln.asIFold)
}

object IFoldAlg {

  type Aux[P[_], Q2[_], I, A] = IFoldAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I, A](
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
