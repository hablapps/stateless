package org.hablapps.stateless
package smonocle
package nat

import scalaz.{ Monad, State, StateT, ~> }
import scalaz.Id.Id
import scalaz.syntax.monad._

import monocle.{ Lens => MLens }

import core.nat.LensAlg

trait LensState {

  type LensField[P[_], A] = LensAlg[P, State[A, ?], A]

  type Lens[S, A] = LensAlg[State[S, ?], State[A, ?], A]

  implicit def asLensAlg[F[_]: Monad, S, A](
      ln: MLens[S, A]): LensAlg[StateT[F, S, ?], StateT[F, A, ?], A] =
    LensAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      λ[StateT[F, A, ?] ~> StateT[F, S, ?]] { sa =>
        StateT(s => sa.xmap(ln.set(_)(s))(ln.get)(s))
      })

  implicit def asLens[S, A](ln: MLens[S, A]): Lens[S, A] =
    asLensAlg[Id, S, A](ln)

  implicit def asLensField[F[_]: Monad, S, A](
      ln: MLens[S, A]): LensField[StateT[F, S, ?], A] =
    LensAlg[StateT[F, S, ?], State[A, ?], A](
      λ[State[A, ?] ~> StateT[F, S, ?]] { sa =>
        StateT(s => sa.xmap(ln.set(_)(s))(ln.get)(s).point[F])
      })
}
