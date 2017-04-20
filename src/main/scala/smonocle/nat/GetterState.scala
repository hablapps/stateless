package org.hablapps.stateless
package smonocle
package nat

import scalaz.{ Monad, State, StateT, ~> }
import scalaz.Id.Id
import scalaz.syntax.monad._
import scalaz.std.tuple._

import monocle.{ Getter => MGetter }

import core.nat.GetterAlg
import core.stateTMonadReader

trait GetterState {

  // type GetterField[P[_], TID] = GetterAlg[P, State[TID, ?], TID]
  //
  // type Getter[S, TID] = GetterAlg[State[S, ?], State[TID, ?], TID]
  //
  // implicit def asGetterAlg[F[_]: Monad, S, A](
  //     gt: MGetter[S, A]): GetterAlg[StateT[F, S, ?], StateT[F, A, ?], A] =
  //   GetterAlg[StateT[F, S, ?], StateT[F, A, ?], A](
  //     λ[StateT[F, A, ?] ~> StateT[F, S, ?]] { sa =>
  //       StateT(s => sa(gt.get(s)).map(_.swap.as(s).swap))
  //     })(implicitly, stateTMonadReader[F, A])
  //
  // implicit def asGetter[S, A](gt: MGetter[S, A]): Getter[S, A] =
  //   asGetterAlg[Id, S, A](gt)
  //
  // implicit def asGetterField[F[_]: Monad, S, A](
  //     gt: MGetter[S, A]): GetterField[StateT[F, S, ?], A] =
  //   GetterAlg[StateT[F, S, ?], State[A, ?], A](
  //     λ[State[A, ?] ~> StateT[F, S, ?]] { sa =>
  //       StateT(s => sa(gt.get(s)).swap.as(s).swap.point[F])
  //     })(implicitly, stateTMonadReader[Id, A])
}
