package org.hablapps.phoropter
package core

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.list._
import scalaz.std.option._

trait MonadPrism[P[_], Q[_], A] extends Monad[P] {

  implicit val MS: MonadState[Q, A]
  val hom: Q ~> λ[x => P[Option[x]]]

  /* derived algebra */

  def getOption: P[Option[A]] = hom(MS.get)

  def set(a: A): P[Unit] = map(hom(MS.put(a)))(_.get) // safe by laws

  def modifyOption(f: A => A): P[Option[Unit]] = hom(MS.modify(f))

  def modify(f: A => A): P[Unit] = map(modifyOption(f))(_ => ())

  def isEmpty: P[Boolean] = map(getOption)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(getOption)(_.nonEmpty)

  def find(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def exist(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def all(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(true)(p))

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

  def composeOptional[R[_], B](op: MonadOptional[Q, R, B]): MonadOptional[P, R, B] =
    MonadOptional(λ[R ~> λ[x => P[Option[x]]]] { rx =>
      map(hom(op.hom(rx)))(_.join)
    })(this, op.MS)

  def composePrism[R[_], B](pr: MonadPrism[Q, R, B]): MonadPrism[P, R, B] =
    MonadPrism(λ[R ~> λ[x => P[Option[x]]]] { rx =>
      map(hom(pr.hom(rx)))(_.join)
    })(this, pr.MS)

  def composeLens[R[_], B](ln: MonadLens[Q, R, B]): MonadOptional[P, R, B] =
    MonadOptional(hom compose ln.hom)(this, ln.MS)
}

object MonadPrism {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[Option[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new MonadPrism[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MS = ev1
    val hom = hom2
  }
}
