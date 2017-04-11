package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.Id.Id
import scalaz.syntax.functor._

trait ILensAlg[P[_], Q[_], I, A] extends raw.ILensAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadState, Id] {

  def get: P[(I, A)] = hom(ev.get.strengthL)

  def set(a: A): P[Unit] = hom(_ => ev.put(a))

  /* composing algebras */

  def composeIOptional[R[_], J, B](
      opt: IOptionalAlg[Q, R, J, B]): IOptionalAlg[P, R, (I, J), B] =
    IOptionalAlg[P, R, (I, J), B](
      λ[λ[x => ((I, J)) => R[x]] ~> λ[y => P[Option[y]]]] { irx =>
        hom(i => opt.hom(j => irx((i, j))))
      }
    )(this, opt.ev)
}

object ILensAlg {

  def apply[P[_], Q[_], I, A](
      hom2: λ[x => I => Q[x]] ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new ILensAlg[P, Q, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
