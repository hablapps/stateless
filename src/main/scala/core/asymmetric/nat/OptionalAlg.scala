package org.hablapps.stateless
package core
package asymmetric
package nat

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._
import scalaz.std.option._

trait OptionalAlg[P[_], Q[_], A] extends OpticAlg[P, Q, A, MonadState, Option]
    with raw.OptionalAlg[P, A] {

  def getOption: P[Option[A]] = hom(ev.get)

  def setOption(a: A): P[Option[Unit]] = hom(ev.put(a))

  /* composing algebras */

  def composeFold[R[_], B](fl: FoldAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(fl.hom(rx)))(_.toList.join)
    })(this, fl.ev)

  def composeGetter[R[_], B](gt: GetterAlg[Q, R, B]): FoldAlg[P, R, B] =
    FoldAlg(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(gt.hom(rx)))(_.toList)
    })(this, gt.ev)

  def composeSetter[R[_], B](st: SetterAlg[Q, R, B]): SetterAlg[P, R, B] =
    SetterAlg(λ[R ~> λ[x => P[Const[Unit, x]]]] { rx =>
      map(hom(st.hom(rx)))(_ => Const(()))
    })(this, st.ev)

  def composeTraversal[R[_], B](tr: TraversalAlg[Q, R, B]): TraversalAlg[P, R, B] =
    TraversalAlg(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(tr.hom(rx)))(_.toList.join)
    })(this, tr.ev)

  def composeOptional[R[_], B](op: OptionalAlg[Q, R, B]): OptionalAlg[P, R, B] =
    OptionalAlg(λ[R ~> λ[x => P[Option[x]]]] { rx =>
      map(hom(op.hom(rx)))(_.join)
    })(this, op.ev)

  def composePrism[R[_], B](pr: PrismAlg[Q, R, B]): OptionalAlg[P, R, B] =
    OptionalAlg(λ[R ~> λ[x => P[Option[x]]]] { rx =>
      map(hom(pr.hom(rx)))(_.join)
    })(this, pr.ev)

  def composeLens[R[_], B](ln: LensAlg[Q, R, B]): OptionalAlg[P, R, B] =
    OptionalAlg(hom compose ln.hom)(this, ln.ev)

  /* transforming algebras */

  def asTraversal: TraversalAlg[P, Q, A] =
    TraversalAlg(λ[Q ~> λ[x => P[List[x]]]] { qx =>
      map(hom(qx))(_.toList)
    })(this, ev)

  def asSetter: SetterAlg[P, Q, A] = asTraversal.asSetter

  def asFold: FoldAlg[P, Q, A] = asTraversal.asFold
}

object OptionalAlg {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[Option[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new OptionalAlg[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
