package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Functor, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._

import shapeless.HNil

trait TraversalAlg[P[_], A] extends OpticAlg[P, A, MonadState, List]
    with raw.TraversalAlg[P, A] {

  def getList: P[List[A]] = hom(ev.get)

  def modifyList(f: A => A): P[List[Unit]] = hom(ev.modify(f))

  /* composing algebras */

  def composeFold[B](fl: FoldAlg[Q, B]): FoldAlg.Aux[P, fl.Q, B] =
    asFold.composeFold(fl)

  def composeGetter[B](gt: GetterAlg[Q, B]): FoldAlg.Aux[P, gt.Q, B] =
    asFold.composeFold(gt.asFold)

  def composeSetter[B](st: SetterAlg[Q, B]): SetterAlg.Aux[P, st.Q, B] =
    asSetter.composeSetter(st)

  def composeTraversal[B](tr: TraversalAlg[Q, B]): TraversalAlg.Aux[P, tr.Q, B] =
    TraversalAlg(λ[tr.Q ~> λ[x => P[List[x]]]] { rx =>
      map(hom(tr.hom(rx)))(_.toList.join)
    })(this, tr.ev)

  def composeOptional[B](op: OptionalAlg[Q, B]): TraversalAlg.Aux[P, op.Q, B] =
    composeTraversal(op.asTraversal)

  def composeLens[B](ln: LensAlg[Q, B]): TraversalAlg.Aux[P, ln.Q, B] =
    composeTraversal(ln.asTraversal)

  /* transforming algebras */

  def asFold: FoldAlg.Aux[P, Q, A] = FoldAlg(hom)(this, ev)

  def asSetter: SetterAlg.Aux[P, Q, A] =
    SetterAlg(λ[Q ~> λ[x => P[Const[Unit, x]]]] { qx =>
      map(hom(qx))(_ => Const(()))
    })(this, ev)

  def asIndexed: ITraversalAlg.Aux[P, Q, HNil, A] =
    ITraversalAlg(λ[λ[x => HNil => Q[x]] ~> λ[x => P[List[x]]]] { iqx =>
      hom(iqx(HNil))
    })(this, ev)

  def asSymmetric: STraversalAlg.Aux[P, Q, Q, A, A] =
    STraversalAlg(hom, hom)(this, ev, ev)

  /* laws */

  trait NatTraversalAlgLaw extends TraversalAlgLaw with OpticAlgLaw

  def natTraversalAlgLaw = new NatTraversalAlgLaw {}
}

object TraversalAlg {

  type Aux[P[_], Q2[_], A] = TraversalAlg[P, A] { type Q[x] = Q2[x] }

  private val fev1 = Functor[List]

  def apply[P[_], Q2[_], A](
      hom2: Q2 ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, A] = new TraversalAlg[P, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    implicit val fev = fev1
    val hom = hom2
  }
}
