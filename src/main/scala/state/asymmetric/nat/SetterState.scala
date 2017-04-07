package org.hablapps.phoropter
package state
package asymmetric
package nat

import scalaz.{ Const, Monad, State, StateT, ~> }
import scalaz.syntax.functor._
import scalaz.std.tuple._

import monocle.{ Lens, Setter }

import core.asymmetric.nat.SetterAlg

trait SetterState {

  // XXX: can't provide an instance for `StateT` by using only a `Setter`
  def fromSetter[S, A](
      st: Setter[S, A]): SetterAlg[State[S, ?], State[A, ?], A] =
    SetterAlg[State[S, ?], State[A, ?], A](
      λ[State[A, ?] ~> λ[x => State[S, Const[Unit, x]]]] { sa =>
        State(s => (st.modify(sa.exec)(s), Const(())))
      })

  def fromSetter[F[_]: Monad, S, A](
      ln: Lens[S, A]): SetterAlg[StateT[F, S, ?], StateT[F, A, ?], A] =
    SetterAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      λ[StateT[F, A, ?] ~> λ[x => StateT[F, S, Const[Unit, x]]]] { sa =>
        StateT(s => sa.xmap(ln.set(_)(s))(ln.get)(s).map(_.map(_ => Const(()))))
      })
}
