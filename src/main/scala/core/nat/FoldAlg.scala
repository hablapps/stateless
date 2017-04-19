package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._

trait FoldAlg[P[_], Q[_], A] extends OpticAlg[P, Q, A, MonadReader, List]
    with raw.FoldAlg[P, A] {

  def getList: P[List[A]] = hom(ev.ask)

  /* composing algebras */

  def composeFold[R[_], B](fl: FoldAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]](rx => map(hom(fl.hom(rx)))(_.join)))(this, fl.ev)

  def composeGetter[R[_], B](gt: GetterAlg[Q, R, B]): FoldAlg[P, R, B] =
    composeFold(gt.asFold)

  def composeTraversal[R[_], B](tr: TraversalAlg[Q, R, B]): FoldAlg[P, R, B] =
    composeFold(tr.asFold)

  def composeOptional[R[_], B](op: OptionalAlg[Q, R, B]): FoldAlg[P, R, B] =
    composeFold(op.asFold)

  def composeLens[R[_], B](ln: LensAlg[Q, R, B]): FoldAlg[P, R, B] =
    composeFold(ln.asFold)

  /* transforming algebras */

  def asIndexed: IFoldAlg[P, Q, Unit, A] =
    IFoldAlg(λ[λ[x => Unit => Q[x]] ~> λ[x => P[List[x]]]] { iqx =>
      hom(iqx(()))
    })(this, ev)

  def asSymmetric: SFoldAlg[P, Q, Q, A, A] = SFoldAlg(hom, hom)(this, ev, ev)
}

object FoldAlg {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q, A]) = new FoldAlg[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
