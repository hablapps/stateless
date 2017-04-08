package org.hablapps.phoropter
package smonocle
package symmetric
package nat

import scalaz.{ Monad, StateT, ~> }

import monocle.Lens

import core.symmetric.nat.SLensAlg

trait SLensState {

  def fromLens[F[_]: Monad, S, A](
      ln: Lens[S, A]): SLensAlg[StateT[F, S, ?], StateT[F, S, ?], StateT[F, A, ?], S, A] =
    SLensAlg[StateT[F, S, ?], StateT[F, S, ?], StateT[F, A, ?], S, A](
      λ[StateT[F, S, ?] ~> StateT[F, S, ?]](ss => ss),
      λ[StateT[F, A, ?] ~> StateT[F, S, ?]] { sa =>
        StateT(s => sa.xmap(ln.set(_)(s))(ln.get)(s))
      })
}
