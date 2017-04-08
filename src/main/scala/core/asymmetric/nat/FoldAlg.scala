package org.hablapps.phoropter
package core
package asymmetric
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
    FoldAlg(hom compose gt.hom)(this, gt.ev)

  def composeTraversal[R[_], B](tr: TraversalAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]](rx => map(hom(tr.hom(rx)))(_.join)))(this, tr.ev)

  def composeOptional[R[_], B](op: OptionalAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(op.map(op.hom(rx))(_.toList)))(_.join)
    })(this, op.ev)

  def composePrism[R[_], B](pr: PrismAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(pr.map(pr.hom(rx))(_.toList)))(_.join)
    })(this, pr.ev)

  def composeLens[R[_], B](ln: LensAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(hom compose ln.hom)(this, ln.ev)
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
