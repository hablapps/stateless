package org.hablapps.phoropter
package state

import scalaz._, Scalaz._
import Kleisli.kleisliMonadReader

import monocle.Fold

import core.MonadFold

trait StateFold {

  def fromFold[F[_]: Monad, S, A](
      fl: Fold[S, A]): MonadFold[ReaderT[F, S, ?], ReaderT[F, A, ?], A] = {
    new MonadFold[ReaderT[F, S, ?], ReaderT[F, A, ?], A] {

      def point[X](x: => X) = kleisliMonadReader.point(x)

      def bind[X, Y](fx: ReaderT[F, S, X])(f: X => ReaderT[F, S, Y]) =
        kleisliMonadReader.bind(fx)(f)

      implicit val MR = kleisliMonadReader

      val hom = λ[ReaderT[F, A, ?] ~> λ[x => ReaderT[F, S, List[x]]]] {
        ra => ReaderT(s => fl.getAll(s).traverse(ra.run))
      }
    }
  }
}
