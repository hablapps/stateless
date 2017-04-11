package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.std.list._
import scalaz.syntax.monad._

trait IFoldAlg[P[_], Q[_], I, A] extends raw.IFoldAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadReader, List] {

  def getList: P[List[(I, A)]] = hom(ev.ask.strengthL)

  /* composing algebras */

  def composeIFold[R[_], J, B](fl: IFoldAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
    IFoldAlg(λ[λ[x => ((I, J)) => R[x]] ~> λ[x => P[List[x]]]] { iqx =>
      map(hom(i => fl.hom(j => iqx((i, j)))))(_.join)
    })(this, fl.ev)

  def composeIGetter[R[_], J, B](gt: IGetterAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
    composeIFold(gt.asIFold)

  def composeITraversal[R[_], J, B](tr: ITraversalAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
    composeIFold(tr.asIFold)

  def composeIOptional[R[_], J, B](op: IOptionalAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
    composeIFold(op.asIFold)

  def composeIPrism[R[_], J, B](pr: IPrismAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
    composeIFold(pr.asIFold)

  def composeILens[R[_], J, B](ln: ILensAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
    composeIFold(ln.asIFold)
}

object IFoldAlg {

  def apply[P[_], Q[_], I, A](
      hom2: λ[x => I => Q[x]] ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q, A]) = new IFoldAlg[P, Q, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
