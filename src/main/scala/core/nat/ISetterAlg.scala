package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.Leibniz.===

import shapeless._, ops.hlist._

trait ISetterAlg[P[_], I <: HList, A] extends raw.ISetterAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadState, Const[Unit, ?]] {

  def modify(f: A => A): P[Unit] = map(hom(_ => ev.modify(f)))(_.getConst)

  def composeSetter[J <: HList, K <: HList, B](
      st: ISetterAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ISetterAlg.Aux[P, st.Q, K, B] =
    ISetterAlg(λ[λ[x => K => st.Q[x]] ~> λ[x => P[Const[Unit, x]]]] { iqx =>
      map(hom(i => st.hom(j => iqx(i ++ j))))(_ => Const(()))
    })(this, st.ev)

  def composeTraversal[J <: HList, K <: HList, B](
      tr: ITraversalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ISetterAlg.Aux[P, tr.Q, K, B] =
    composeSetter(tr.asSetter)

  def composeOptional[J <: HList, K <: HList, B](
      op: IOptionalAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ISetterAlg.Aux[P, op.Q, K, B] =
    composeSetter(op.asSetter)

  def composeLens[J <: HList, K <: HList, B](
      ln: ILensAlg[Q, J, B])(implicit
      ev0: Prepend.Aux[I, J, K]): ISetterAlg.Aux[P, ln.Q, K, B] =
    composeSetter(ln.asSetter)

  /* transforming algebras */

  def asPlain(implicit ev0: I === HNil): SetterAlg.Aux[P, Q, A] =
    SetterAlg[P, Q, A](λ[Q ~> λ[x => P[Const[Unit, x]]]] { qx =>
      hom(_ => qx)
    })(this, ev)
}

object ISetterAlg {

  type Aux[P[_], Q2[_], I <: HList, A] = ISetterAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I <: HList, A](
      hom2: λ[x => I => Q2[x]] ~> λ[x => P[Const[Unit, x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, I, A] = new ISetterAlg[P, I, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }

  implicit def toIndexed[P[_], A](st: SetterAlg[P, A]): Aux[P, st.Q, HNil, A] =
    st.asIndexed
}
