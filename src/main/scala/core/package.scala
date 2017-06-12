package org.hablapps.stateless

// import scalaz.{ Const, Monad, MonadState, MonadReader, StateT }
// import scalaz.syntax.monad._

import scalaz._, Scalaz._

package object `core` {

  // XXX: this has to be standard somewhere
  implicit def fromStateToReader[F[_], A](ms: MonadState[F, A]): MonadReader[F, A] =
    new MonadReader[F, A] {
      def point[X](x: => X) = ms.point(x)
      def bind[X, Y](fx: F[X])(f: X => F[Y]): F[Y] = ms.bind(fx)(f)
      def ask = ms.get
      def local[X](f: A => A)(fx: F[X]): F[X] = {
        implicit val ims: MonadState[F, A] = ms
        for {
          a <- ms.get
          _ <- ms.put(f(a))
          x <- fx
          _ <- ms.put(a)
        } yield x
      }
    }

  implicit def stateTMonadReader[F[_]: Monad, A]: MonadReader[StateT[F, A, ?], A] =
    fromStateToReader[StateT[F, A, ?], A](MonadState[StateT[F, A, ?], A])
}
