// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Monad, ~> }
//
// trait SOpticAlg[P[_], L[_], R[_], A, B, Ev[M[_], _] <: Monad[M], F[_]] extends Monad[P] {
//
//   implicit val evL: Ev[L, A]
//   implicit val evR: Ev[R, B]
//
//   val homL: L ~> λ[x => P[F[x]]]
//   val homR: R ~> λ[x => P[F[x]]]
// }
