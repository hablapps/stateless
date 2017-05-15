package org.hablapps.stateless
package smonocle
package nat

import scalaz._, Scalaz._

import monocle.Fold

import core.nat.FoldAlg

trait FoldState {

  def fromFoldable[F[_]: Foldable, A]
      : FoldAlg.Aux[Reader[F[A], ?], Reader[A, ?], A] =
    FoldAlg[Reader[F[A], ?], Reader[A, ?], A](
      λ[Reader[A, ?] ~> λ[x => Reader[F[A], List[x]]]] { ra =>
        Reader(_.foldMap(a => List(ra.run(a))))
      })

  def fromFold[F[_]: Monad, S, A](
      fl: Fold[S, A]): FoldAlg.Aux[ReaderT[F, S, ?], ReaderT[F, A, ?], A] =
    FoldAlg[ReaderT[F, S, ?], ReaderT[F, A, ?], A](
      λ[ReaderT[F, A, ?] ~> λ[x => ReaderT[F, S, List[x]]]] { ra =>
        ReaderT(s => fl.getAll(s).traverse(ra.run))
      })
}
