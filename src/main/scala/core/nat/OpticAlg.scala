package org.hablapps.stateless
package core
package nat

import scalaz.{ Foldable, Functor, Monad, MonadReader, MonadState, Monoid, ~> }
import scalaz.syntax.foldable._

trait OpticAlg[P[_], A, Ev[M[_], _] <: Monad[M], F[_]] extends Monad[P] {

  type Q[_]

  implicit val ev: Ev[Q, A]

  implicit val fev: Functor[F]

  val hom: Q ~> λ[x => P[F[x]]]
}

trait MROpticAlg[P[_], A, F[_]] extends OpticAlg[P, A, MonadReader, F] {

  def view: P[F[A]] = hom(ev.ask)

  def fold[M: Monoid](f: A => M)(implicit ev: Foldable[F]): P[M] =
    map(view)(_.foldMap(f))
}

trait MSOpticAlg[P[_], A, F[_]] extends OpticAlg[P, A, MonadState, F] {

  def view: P[F[A]] = hom(ev.get)

  def set(a: A): P[F[Unit]] = hom(ev.put(a))

  def modi(f: A => A): P[F[Unit]] = hom(ev.modify(f))

  def fold[M: Monoid](f: A => M)(implicit ev: Foldable[F]): P[M] =
    map(view)(_.foldMap(f))
}
