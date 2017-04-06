package org.hablapps.phoropter
package core

import scalaz.{ Const, Monad, MonadState, Monoid, ~> }
import scalaz.syntax.foldable._
import scalaz.syntax.monad._
import scalaz.std.list._

trait MonadTraversal[P[_], Q[_], A] extends Monad[P] {

  implicit val MS: MonadState[Q, A]
  val hom: Q ~> λ[x => P[List[x]]]

  /* derived algebra */

  def getAll: P[List[A]] = hom(MS.get)

  def foldMap[M: Monoid](f: A => M): P[M] = map(getAll)(_.foldMap(f))

  def find(p: A => Boolean): P[Option[A]] = map(getAll)(_.find(p))

  def headOption: P[Option[A]] = map(getAll)(_.headOption)

  def lastOption: P[Option[A]] = map(getAll)(_.lastOption)

  def exist(p: A => Boolean): P[Boolean] = map(getAll)(_.exists(p))

  def all(p: A => Boolean): P[Boolean] = map(getAll)(_.all(p))

  def length: P[Int] = map(getAll)(_.length)

  def isEmpty: P[Boolean] = map(getAll)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(isEmpty)(! _)

  def modify(f: A => A): P[Unit] = map(hom(MS.modify(f)))(_ => ())

  def set(a: A): P[Unit] = map(hom(MS.put(a)))(_ => ())

  /* composing algebras */

  def composeFold[R[_], B](fl: MonadFold[Q, R, B]): MonadFold[P, R, B] =
    MonadFold(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(fl.hom(rx)))(_.toList.join)
    })(this, fl.MR)

  def composeGetter[R[_], B](gt: MonadGetter[Q, R, B]): MonadFold[P, R, B] =
    MonadFold(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(gt.hom(rx)))(_.toList)
    })(this, gt.MR)

  def composeSetter[R[_], B](st: MonadSetter[Q, R, B]): MonadSetter[P, R, B] =
    MonadSetter(λ[R ~> λ[x => P[Const[Unit, x]]]] { rx =>
      map(hom(st.hom(rx)))(_ => Const(()))
    })(this, st.MS)

  def composeTraversal[R[_], B](tr: MonadTraversal[Q, R, B]): MonadTraversal[P, R, B] =
    MonadTraversal(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(tr.hom(rx)))(_.toList.join)
    })(this, tr.MS)

  def composeOptional[R[_], B](op: MonadOptional[Q, R, B]): MonadTraversal[P, R, B] =
    MonadTraversal(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(op.map(op.hom(rx))(_.toList)))(_.join)
    })(this, op.MS)

  def composePrism[R[_], B](pr: MonadPrism[Q, R, B]): MonadTraversal[P, R, B] =
    MonadTraversal(λ[R ~> λ[x => P[List[x]]]] { rx =>
      map(hom(pr.map(pr.hom(rx))(_.toList)))(_.join)
    })(this, pr.MS)

  def composeLens[R[_], B](ln: MonadLens[Q, R, B]): MonadTraversal[P, R, B] =
    MonadTraversal(hom compose ln.hom)(this, ln.MS)
}

object MonadTraversal {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new MonadTraversal[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MS = ev1
    val hom = hom2
  }
}
