package org.hablapps.stateless
package smonocle
package nat

import scalaz._, Scalaz._

import monocle.Fold

import core.nat.FoldAlg

trait FoldState {

  def fromFold[F[_]: Monad, S, A](
      fl: Fold[S, A]): FoldAlg[ReaderT[F, S, ?], ReaderT[F, A, ?], A] =
    FoldAlg[ReaderT[F, S, ?], ReaderT[F, A, ?], A](
      λ[ReaderT[F, A, ?] ~> λ[x => ReaderT[F, S, List[x]]]] { ra =>
        ReaderT(s => fl.getAll(s).traverse(ra.run))
      })
}
