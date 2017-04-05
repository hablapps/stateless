package org.hablapps.phoropter
package state

import scalaz.{ Monad, ReaderT, ~> }
import scalaz.Kleisli.kleisliMonadReader

import monocle.Getter

import core.MonadGetter

trait StateGetter {

  def fromGetter[F[_]: Monad, S, A](
      gt: Getter[S, A]): MonadGetter[ReaderT[F, S, ?], ReaderT[F, A, ?], A] = {
    new MonadGetter[ReaderT[F, S, ?], ReaderT[F, A, ?], A] {

      def point[X](x: => X) = kleisliMonadReader.point(x)

      def bind[X, Y](fx: ReaderT[F, S, X])(f: X => ReaderT[F, S, Y]) =
        kleisliMonadReader.bind(fx)(f)

      // FIXME: dummy implementation
      def local[X](f: A => A)(fx: ReaderT[F, S, X]) = fx

      implicit val MR = kleisliMonadReader

      val hom = λ[ReaderT[F, A, ?] ~> ReaderT[F, S, ?]] {
        ra => ReaderT(s => ra.run(gt.get(s)))
      }
    }
  }
}
