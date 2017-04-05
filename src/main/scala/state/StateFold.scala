package org.hablapps.phoropter
package state

import scalaz._, Scalaz._

import monocle.Fold

import core.MonadFold

trait StateFold {

  def fromFold[F[_]: Monad, S, A](
      fl: Fold[S, A]): MonadFold[ReaderT[F, S, ?], ReaderT[F, A, ?], A] =
    MonadFold[ReaderT[F, S, ?], ReaderT[F, A, ?], A](
      λ[ReaderT[F, A, ?] ~> λ[x => ReaderT[F, S, List[x]]]] { ra =>
        ReaderT(s => fl.getAll(s).traverse(ra.run))
      })
}
