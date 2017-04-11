package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._

trait TraversalAlg[P[_], Q[_], A] extends OpticAlg[P, Q, A, MonadState, List]
    with raw.TraversalAlg[P, A] {

  def getList: P[List[A]] = hom(ev.get)

  def modifyList(f: A => A): P[List[Unit]] = hom(ev.modify(f))

  /* composing algebras */

  def composeFold[R[_], B](fl: FoldAlg[Q, R, B]): FoldAlg[P, R, B] =
    asFold.composeFold(fl)

  def composeGetter[R[_], B](gt: GetterAlg[Q, R, B]): FoldAlg[P, R, B] =
    asFold.composeFold(gt.asFold)

  def composeSetter[R[_], B](st: SetterAlg[Q, R, B]): SetterAlg[P, R, B] =
    asSetter.composeSetter(st)

  def composeTraversal[R[_], B](tr: TraversalAlg[Q, R, B]): TraversalAlg[P, R, B] =
    TraversalAlg(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(tr.hom(rx)))(_.toList.join)
    })(this, tr.ev)

  def composeOptional[R[_], B](op: OptionalAlg[Q, R, B]): TraversalAlg[P, R, B] =
    composeTraversal(op.asTraversal)

  def composePrism[R[_], B](pr: PrismAlg[Q, R, B]): TraversalAlg[P, R, B] =
    composeTraversal(pr.asTraversal)

  def composeLens[R[_], B](ln: LensAlg[Q, R, B]): TraversalAlg[P, R, B] =
    composeTraversal(ln.asTraversal)

  /* transforming algebras */

  def asFold: FoldAlg[P, Q, A] = FoldAlg(hom)(this, ev)

  def asSetter: SetterAlg[P, Q, A] =
    SetterAlg(λ[Q ~> λ[x => P[Const[Unit, x]]]] { qx =>
      map(hom(qx))(_ => Const(()))
    })(this, ev)

  def asIndexed: ITraversalAlg[P, Q, Unit, A] =
    ITraversalAlg(λ[λ[x => Unit => Q[x]] ~> λ[x => P[List[x]]]] { iqx =>
      hom(iqx(()))
    })(this, ev)

  def asSymmetric: STraversalAlg[P, Q, Q, A, A] =
    STraversalAlg(hom, hom)(this, ev, ev)
}

object TraversalAlg {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new TraversalAlg[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
