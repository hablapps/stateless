package org.hablapps.phoropter
package core

import scalaz.{ Functor, MonadReader, MonadState }
import scalaz.syntax.functor._

trait Getter[P[_], A] {

  def get: P[A]

  // derived methods

  def gets[B](f: A => B)(implicit F: Functor[P]): P[B] = get.map(f)
}

object Getter extends GetterInstances {
  implicit def stateInstance[M[_], A](implicit MS: MonadState[M, A]): Getter[M, A] =
    new Getter[M, A] { def get = MS.get }
}

trait GetterInstances {
  implicit def readerInstance[M[_], A](implicit MR: MonadReader[M, A]): Getter[M, A] =
    new Getter[M, A] { def get = MR.ask }
}
