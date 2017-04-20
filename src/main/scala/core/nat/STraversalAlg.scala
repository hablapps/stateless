// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Monad, MonadState, ~> }
//
// trait STraversalAlg[P[_], L[_], R[_], A, B] extends raw.STraversalAlg[P, B]
//     with SOpticAlg[P, L, R, A, B, MonadState, List] {
//
//   def getListL: P[List[A]] = homL[A](evL.get)
//
//   def modifyListL(f: A => A): P[List[Unit]] = homL(evL.modify(f))
//
//   def getListR: P[List[B]] = homR[B](evR.get)
//
//   def modifyListR(f: B => B): P[List[Unit]] = homR(evR.modify(f))
// }
//
// object STraversalAlg {
//
//   def apply[P[_], L[_], R[_], A, B](
//       homL2: L ~> λ[x => P[List[x]]],
//       homR2: R ~> λ[x => P[List[x]]])(implicit
//       ev0: Monad[P],
//       ev1: MonadState[L, A],
//       ev2: MonadState[R, B]) = new STraversalAlg[P, L, R, A, B] {
//     def point[X](x: => X) = ev0.point(x)
//     def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
//     implicit val evL = ev1
//     implicit val evR = ev2
//     val homL = homL2
//     val homR = homR2
//   }
// }
