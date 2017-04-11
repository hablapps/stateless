package org.hablapps.stateless
package smonocle
package nat

import scalaz.{ Monad, StateT, ~> }

import monocle.Lens

import core.nat.SLensAlg

trait SLensState {

  def fromSLens[F[_]: Monad, S, A](
      ln: Lens[S, A]): SLensAlg[StateT[F, S, ?], StateT[F, S, ?], StateT[F, A, ?], S, A] =
    SLensAlg[StateT[F, S, ?], StateT[F, S, ?], StateT[F, A, ?], S, A](
      λ[StateT[F, S, ?] ~> StateT[F, S, ?]](ss => ss),
      λ[StateT[F, A, ?] ~> StateT[F, S, ?]] { sa =>
        StateT(s => sa.xmap(ln.set(_)(s))(ln.get)(s))
      })
}
