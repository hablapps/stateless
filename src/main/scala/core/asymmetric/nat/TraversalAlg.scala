package org.hablapps.phoropter
package core
package asymmetric
package nat

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._

trait TraversalAlg[P[_], Q[_], A] extends OpticAlg[P, Q, A, MonadState, List]
    with raw.TraversalAlg[P, A] {

  def getAll: P[List[A]] = hom(ev.get)

  def modifyList(f: A => A): P[List[Unit]] = hom(ev.modify(f))

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

  def composeOptional[R[_], B](op: OptionalAlg[Q, R, B]): TraversalAlg[P, R, B] =
    TraversalAlg(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(op.map(op.hom(rx))(_.toList)))(_.join)
    })(this, op.ev)

  def composePrism[R[_], B](pr: PrismAlg[Q, R, B]): TraversalAlg[P, R, B] =
    TraversalAlg(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(pr.map(pr.hom(rx))(_.toList)))(_.join)
    })(this, pr.ev)

  def composeLens[R[_], B](ln: LensAlg[Q, R, B]): TraversalAlg[P, R, B] =
    TraversalAlg(hom compose ln.hom)(this, ln.ev)
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
