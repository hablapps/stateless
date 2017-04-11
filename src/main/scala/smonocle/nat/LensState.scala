package org.hablapps.stateless
package smonocle
package nat

import scalaz.{ Monad, StateT, ~> }

import monocle.Lens

import core.nat.LensAlg

trait LensState {

  def fromLens[F[_]: Monad, S, A](
      ln: Lens[S, A]): LensAlg[StateT[F, S, ?], StateT[F, A, ?], A] =
    LensAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      λ[StateT[F, A, ?] ~> StateT[F, S, ?]] { sa =>
        StateT(s => sa.xmap(ln.set(_)(s))(ln.get)(s))
      })
}
