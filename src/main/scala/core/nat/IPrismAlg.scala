package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.syntax.functor._

trait IPrismAlg[P[_], Q[_], I, A] extends raw.IPrismAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadState, Option] {

  def getOption: P[Option[(I, A)]] = hom(ev.get.strengthL)

  def set(a: A): P[Unit] = map(hom(_ => ev.put(a)))(_.get)

  /* transforming algebras */

  def asIOptional: IOptionalAlg[P, Q, I, A] = IOptionalAlg(hom)(this, ev)

  def asITraversal: ITraversalAlg[P, Q, I, A] = asIOptional.asITraversal

  def asISetter: ISetterAlg[P, Q, I, A] = asITraversal.asISetter

  def asIFold: IFoldAlg[P, Q, I, A] = asITraversal.asIFold
}

object IPrismAlg {

  def apply[P[_], Q[_], I, A](
      hom2: λ[x => I => Q[x]] ~> λ[y => P[Option[y]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new IPrismAlg[P, Q, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
