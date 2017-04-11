package org.hablapps.stateless

import scalaz.{ Const, Monad, MonadState, MonadReader, StateT }
import scalaz.syntax.monad._

package object `core` {

  // XXX: this has to be standard somewhere
  implicit def fromStateToReader[F[_], A](ms: MonadState[F, A]): MonadReader[F, A] =
    new MonadReader[F, A] {
      def point[X](x: => X) = ms.point(x)
      def bind[X, Y](fx: F[X])(f: X => F[Y]): F[Y] = ms.bind(fx)(f)
      def ask = ms.get
      def local[X](f: A => A)(fx: F[X]): F[X] = ms.bind(ms.modify(f))(_ => fx)
    }
}
