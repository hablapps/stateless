package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Functor, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.option._

import shapeless.HNil

trait SetterAlg[P[_], A] extends OpticAlg[P, A, MonadState, Const[Unit, ?]]
    with raw.SetterAlg[P, A] {

  def modify(f: A => A): P[Unit] = map(hom(ev.modify(f)))(_.getConst)

  /* composing algebras */

  def composeSetter[B](st: SetterAlg[Q, B]): SetterAlg.Aux[P, st.Q, B] =
    SetterAlg(λ[st.Q ~> λ[x => P[Const[Unit, x]]]] { rx =>
      map(hom(st.hom(rx)))(_ => Const(()))
    })(this, st.ev)

  def composeTraversal[B](tr: TraversalAlg[Q, B]): SetterAlg.Aux[P, tr.Q, B] =
    composeSetter(tr.asSetter)

  def composeOptional[B](op: OptionalAlg[Q, B]): SetterAlg.Aux[P, op.Q, B] =
    composeSetter(op.asSetter)

  def composeLens[B](ln: LensAlg[Q, B]): SetterAlg.Aux[P, ln.Q, B] =
    composeSetter(ln.asSetter)

  /* transforming algebras */

  def asIndexed: ISetterAlg.Aux[P, Q, HNil, A] =
    ISetterAlg(λ[λ[x => HNil => Q[x]] ~> λ[x => P[Const[Unit, x]]]] { iqx =>
      hom(iqx(HNil))
    })(this, ev)

  def asSymmetric: SSetterAlg.Aux[P, Q, Q, A, A] =
    SSetterAlg(hom, hom)(this, ev, ev)

  /* laws */

  trait NatSetterAlgLaw extends SetterAlgLaw with OpticAlgLaw
}

object SetterAlg {

  type Aux[P[_], Q2[_], A] = SetterAlg[P, A] { type Q[x] = Q2[x] }

  private val fev1 = Functor[Const[Unit, ?]]

  def apply[P[_], Q2[_], A](
      hom2: Q2 ~> λ[x => P[Const[Unit, x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, A] = new SetterAlg[P, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    implicit val fev = fev1
    val hom = hom2
  }
}
