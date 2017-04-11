package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.option._

trait SetterAlg[P[_], Q[_], A] extends OpticAlg[P, Q, A, MonadState, Const[Unit, ?]]
    with raw.SetterAlg[P, A] {

  def modify(f: A => A): P[Unit] = map(hom(ev.modify(f)))(_.getConst)

  /* composing algebras */

  def composeSetter[R[_], B](st: SetterAlg[Q, R, B]): SetterAlg[P, R, B] =
    SetterAlg(λ[R ~> λ[x => P[Const[Unit, x]]]] { rx =>
      map(hom(st.hom(rx)))(_ => Const(()))
    })(this, st.ev)

  def composeTraversal[R[_], B](tr: TraversalAlg[Q, R, B]): SetterAlg[P, R, B] =
    composeSetter(tr.asSetter)

  def composeOptional[R[_], B](op: OptionalAlg[Q, R, B]): SetterAlg[P, R, B] =
    composeSetter(op.asSetter)

  def composePrism[R[_], B](pr: PrismAlg[Q, R, B]): SetterAlg[P, R, B] =
    composeSetter(pr.asSetter)

  def composeLens[R[_], B](ln: LensAlg[Q, R, B]): SetterAlg[P, R, B] =
    composeSetter(ln.asSetter)

  /* transforming algebras */

  def asIndexed: ISetterAlg[P, Q, Unit, A] =
    ISetterAlg(λ[λ[x => Unit => Q[x]] ~> λ[x => P[Const[Unit, x]]]] { iqx =>
      hom(iqx(()))
    })(this, ev)

  def asSymmetric: SSetterAlg[P, Q, Q, A, A] =
    SSetterAlg(hom, hom)(this, ev, ev)
}

object SetterAlg {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[Const[Unit, x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new SetterAlg[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
