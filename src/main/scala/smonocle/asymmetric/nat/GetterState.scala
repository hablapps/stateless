package org.hablapps.stateless
package smonocle
package asymmetric
package nat

import scalaz.{ Monad, ReaderT, ~> }

import monocle.Getter

import core.asymmetric.nat.GetterAlg

trait GetterState {

  def fromGetter[F[_]: Monad, S, A](
      gt: Getter[S, A]): GetterAlg[ReaderT[F, S, ?], ReaderT[F, A, ?], A] =
    GetterAlg[ReaderT[F, S, ?], ReaderT[F, A, ?], A](
      λ[ReaderT[F, A, ?] ~> ReaderT[F, S, ?]] { ra =>
        ReaderT(s => ra.run(gt.get(s)))
      })
}
