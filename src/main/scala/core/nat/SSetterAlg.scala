// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Const, Monad, MonadState, ~> }
//
// trait SSetterAlg[P[_], L[_], R[_], A, B] extends raw.SSetterAlg[P, B]
//     with SOpticAlg[P, L, R, A, B, MonadState, Const[Unit, ?]] {
//
//   def setL(a: A): P[Unit] = map(homL(evL.put(a)))(_.getConst)
//
//   def setR(b: B): P[Unit] = map(homR(evR.put(b)))(_.getConst)
// }
//
// object SSetterAlg {
//
//   def apply[P[_], L[_], R[_], A, B](
//       homL2: L ~> λ[x => P[Const[Unit, x]]],
//       homR2: R ~> λ[x => P[Const[Unit, x]]])(implicit
//       ev0: Monad[P],
//       ev1: MonadState[L, A],
//       ev2: MonadState[R, B]) = new SSetterAlg[P, L, R, A, B] {
//     def point[X](x: => X) = ev0.point(x)
//     def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
//     implicit val evL = ev1
//     implicit val evR = ev2
//     val homL = homL2
//     val homR = homR2
//   }
// }
