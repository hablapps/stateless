package org.hablapps.phoropter
package core

import scalaz.{ Const, Monad, MonadState, Monoid, ~> }
import scalaz.syntax.foldable._
import scalaz.syntax.monad._
import scalaz.std.list._

trait TraversalAlg[P[_], Q[_], A] extends OpticAlg[P, Q, A, MonadState, List] {

  def getAll: P[List[A]] = hom(ev.get)

  def foldMap[M: Monoid](f: A => M): P[M] = map(getAll)(_.foldMap(f))

  def find(p: A => Boolean): P[Option[A]] = map(getAll)(_.find(p))

  def headOption: P[Option[A]] = map(getAll)(_.headOption)

  def lastOption: P[Option[A]] = map(getAll)(_.lastOption)

  def exist(p: A => Boolean): P[Boolean] = map(getAll)(_.exists(p))

  def all(p: A => Boolean): P[Boolean] = map(getAll)(_.all(p))

  def length: P[Int] = map(getAll)(_.length)

  def isEmpty: P[Boolean] = map(getAll)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(isEmpty)(! _)

  def modify(f: A => A): P[Unit] = map(hom(ev.modify(f)))(_ => ())

  def set(a: A): P[Unit] = map(hom(ev.put(a)))(_ => ())

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
