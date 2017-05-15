package org.hablapps.stateless
package smonocle
package nat

import scalaz._, Scalaz._

import monocle.{ Lens, Setter }

import core.nat.SetterAlg

trait SetterState {

  def fromFunctor[F[_]: Functor, A]
      : SetterAlg.Aux[State[F[A], ?], State[A, ?], A] =
    SetterAlg[State[F[A], ?], State[A, ?], A](
      λ[State[A, ?] ~> λ[x => State[F[A], Const[Unit, x]]]] { sa =>
        State(ta => (ta.map(sa.exec), Const(())))
      })

  // XXX: can't provide an instance for `StateT` by using only a `Setter`
  def fromSetter[S, A](
      st: Setter[S, A]): SetterAlg.Aux[State[S, ?], State[A, ?], A] =
    SetterAlg[State[S, ?], State[A, ?], A](
      λ[State[A, ?] ~> λ[x => State[S, Const[Unit, x]]]] { sa =>
        State(s => (st.modify(sa.exec)(s), Const(())))
      })

  def fromSetter[F[_]: Monad, S, A](
      ln: Lens[S, A]): SetterAlg.Aux[StateT[F, S, ?], StateT[F, A, ?], A] =
    SetterAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      λ[StateT[F, A, ?] ~> λ[x => StateT[F, S, Const[Unit, x]]]] { sa =>
        StateT(s => sa.xmap(ln.set(_)(s))(ln.get)(s).map(_.map(_ => Const(()))))
      })
}
