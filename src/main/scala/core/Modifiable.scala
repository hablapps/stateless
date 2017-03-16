package org.hablapps.phoropter
package core

import Function.const
import scalaz.MonadState

trait Modifiable[F[_], A] {

  def modify(f: A => A): F[Unit]

  def put(a: A): F[Unit] = modify(const(a))
}

object Modifiable {
  implicit def stateInstance[M[_], A](implicit MS: MonadState[M, A]): Modifiable[M, A] =
    new Modifiable[M, A] {
      def modify(f: A => A): M[Unit] = MS.modify(f)
    }
}
