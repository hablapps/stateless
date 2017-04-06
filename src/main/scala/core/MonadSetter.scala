package org.hablapps.phoropter
package core

import scalaz.{ Const, Monad, MonadState, ~> }
import scalaz.syntax.monad._
import scalaz.std.option._

trait MonadSetter[P[_], Q[_], A] extends Monad[P] {

  implicit val MS: MonadState[Q, A] // should be restricted to `MonadPut`
  val hom: Q ~> λ[x => P[Const[Unit, x]]]

  /* derived algebra */

  def modify(f: A => A): P[Unit] = map(hom(MS.modify(f)))(_.getConst)

  def set(a: A): P[Unit] = modify(_ => a)

  /* composing algebras */

  def composeSetter[R[_], B](st: MonadSetter[Q, R, B]): MonadSetter[P, R, B] =
    MonadSetter(λ[R ~> λ[x => P[Const[Unit, x]]]] { rx =>
      map(hom(st.hom(rx)))(_ => Const(()))
    })(this, st.MS)

  def composeTraversal[R[_], B](tr: MonadTraversal[Q, R, B]): MonadSetter[P, R, B] =
    MonadSetter(λ[R ~> λ[x => P[Const[Unit, x]]]] { rx =>
      map(hom(tr.hom(rx)))(_ => Const(()))
    })(this, tr.MS)

  def composeOptional[R[_], B](op: MonadOptional[Q, R, B]): MonadSetter[P, R, B] =
    MonadSetter(λ[R ~> λ[x => P[Const[Unit, x]]]] { rx =>
      map(hom(op.hom(rx)))(_ => Const(()))
    })(this, op.MS)

  def composePrism[R[_], B](pr: MonadPrism[Q, R, B]): MonadSetter[P, R, B] =
    MonadSetter(λ[R ~> λ[x => P[Const[Unit, x]]]] { rx =>
      map(hom(pr.hom(rx)))(_ => Const(()))
    })(this, pr.MS)

  def composeLens[R[_], B](ln: MonadLens[Q, R, B]): MonadSetter[P, R, B] =
    MonadSetter(λ[R ~> λ[x => P[Const[Unit, x]]]] { rx =>
      map(hom(ln.hom(rx)))(_ => Const(()))
    })(this, ln.MS)
}

object MonadSetter {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[Const[Unit, x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new MonadSetter[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MS = ev1
    val hom = hom2
  }
}
