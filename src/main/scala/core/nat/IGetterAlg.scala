package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.Id.Id
import scalaz.syntax.functor._

trait IGetterAlg[P[_], I, A] extends raw.IGetterAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadReader, Id] {

  def get: P[(I, A)] = hom(ev.ask.strengthL)

  /* composing algebras */

  def composeIFold[J, B](fl: IFoldAlg[Q, J, B]): IFoldAlg.Aux[P, fl.Q, (I, J), B] =
    asIFold.composeIFold(fl)

  def composeIGetter[J, B](gt: IGetterAlg[Q, J, B]): IGetterAlg.Aux[P, gt.Q, (I, J), B] =
    IGetterAlg(new (λ[x => ((I, J)) => gt.Q[x]] ~> P) {
      def apply[X](iqx: ((I, J)) => gt.Q[X]): P[X] =
        hom[X](i => gt.hom[X](j => iqx((i, j))))
    })(this, gt.ev)

  def composeITraversal[J, B](tr: ITraversalAlg[Q, J, B]): IFoldAlg.Aux[P, tr.Q, (I, J), B] =
    composeIFold(tr.asIFold)

  def composeIOptional[J, B](op: IOptionalAlg[Q, J, B]): IFoldAlg.Aux[P, op.Q, (I, J), B] =
    composeIFold(op.asIFold)

  def composeILens[J, B](ln: ILensAlg[Q, J, B]): IGetterAlg.Aux[P, ln.Q, (I, J), B] =
    composeIGetter(ln.asIGetter)

  /* transforming algebras */

  def asIFold: IFoldAlg.Aux[P, Q, I, A] =
    IFoldAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[List[x]]]] { qx =>
      map(hom(qx))(List(_))
    })(this, ev)
}

object IGetterAlg {

  type Aux[P[_], Q2[_], I, A] = IGetterAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I, A](
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
