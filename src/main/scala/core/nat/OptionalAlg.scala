package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Functor, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._
import scalaz.std.option._

import shapeless.HNil

trait OptionalAlg[P[_], A] extends OpticAlg[P, A, MonadState, Option]
    with raw.OptionalAlg[P, A] {

  implicit val M: Monad[P]

  def getOption: P[Option[A]] = hom(ev.get)

  def setOption(a: A): P[Option[Unit]] = hom(ev.put(a))

  /* composing algebras */

  def composeFold[B](fl: FoldAlg[Q, B]): FoldAlg.Aux[P, fl.Q, B] =
    asFold.composeFold(fl)

  def composeGetter[B](gt: GetterAlg[Q, B]): FoldAlg.Aux[P, gt.Q, B] =
    asFold.composeFold(gt.asFold)

  def composeSetter[B](st: SetterAlg[Q, B]): SetterAlg.Aux[P, st.Q, B] =
    asSetter.composeSetter(st)

  def composeTraversal[B](tr: TraversalAlg[Q, B]): TraversalAlg.Aux[P, tr.Q, B] =
    asTraversal.composeTraversal(tr)

  def composeOptional[B](op: OptionalAlg[Q, B]): OptionalAlg.Aux[P, op.Q, B] =
    OptionalAlg(λ[op.Q ~> λ[x => P[Option[x]]]] { rx =>
      hom(op.hom(rx)) map (_.join)
    })(M, op.ev)

  def composeLens[B](ln: LensAlg[Q, B]): OptionalAlg.Aux[P, ln.Q, B] =
    composeOptional(ln.asOptional)

  /* transforming algebras */

  def asTraversal: TraversalAlg.Aux[P, Q, A] =
    TraversalAlg(λ[Q ~> λ[x => P[List[x]]]] { qx =>
      hom(qx) map (_.toList)
    })(M, ev)

  def asSetter: SetterAlg.Aux[P, Q, A] = asTraversal.asSetter

  def asFold: FoldAlg.Aux[P, Q, A] = asTraversal.asFold

  def asIndexed: IOptionalAlg.Aux[P, Q, HNil, A] =
    IOptionalAlg(λ[λ[x => HNil => Q[x]] ~> λ[x => P[Option[x]]]] { iqx =>
      hom(iqx(HNil))
    })(M, ev)

  /* laws */

  trait NatOptionalAlgLaw extends OptionalAlgLaw

  def natOptionalAlgLaw = new NatOptionalAlgLaw {}
}

object OptionalAlg {

  type Aux[P[_], Q2[_], A] = OptionalAlg[P, A] { type Q[x] = Q2[x] }

  private val fev1 = Functor[Option]

  def apply[P[_], Q2[_], A](
      hom2: Q2 ~> λ[x => P[Option[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, A] = new OptionalAlg[P, A] {
    type Q[x] = Q2[x]
    val M = ev0
    implicit val ev = ev1
    implicit val fev = fev1
    val hom = hom2
  }
}
