package org.hablapps.phoropter
package core
package asymmetric
package nat
package indexed

import scalaz.{ Monad, MonadState, ~> }
import scalaz.syntax.functor._

import op._, At.syntax._

trait IMapAlg[P[_], Q[_], R[_], I, A] extends ITraversalAlg[P, Q, I, A] {

  implicit val ta: At[P, R, I, A]

  // additional algebra

  def add(i: I)(a: A): P[Unit] = putOpt(i)(Option(a))

  def remove(i: I): P[Unit] = putOpt(i)(None)

  def pick[O](i: I)(ro: R[O]): P[O] = at(i).hom[O](ro)

  def get(i: I): P[Option[A]] = at(i).get

  private def putOpt(i: I)(oa: Option[A]): P[Unit] = at(i).hom(at(i).ev.put(oa))
}

object IMapAlg {

  def apply[P[_], Q[_], R[_], I, A](
      hom2: λ[x => I => Q[x]] ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A],
      ev2: At[P, R, I, A]) = new IMapAlg[P, Q, R, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
    implicit val ta = ev2
  }
}
