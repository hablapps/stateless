package org.hablapps.phoropter
package state

import scalaz.{ Monad, ReaderT, ~> }

import monocle.Getter

import core.MonadGetter

trait StateGetter {

  def fromGetter[F[_]: Monad, S, A](
      gt: Getter[S, A]): MonadGetter[ReaderT[F, S, ?], ReaderT[F, A, ?], A] =
    MonadGetter[ReaderT[F, S, ?], ReaderT[F, A, ?], A](
      λ[ReaderT[F, A, ?] ~> ReaderT[F, S, ?]] { ra =>
        ReaderT(s => ra.run(gt.get(s)))
      })
}
