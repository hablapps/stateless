// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Monad, MonadState, ~> }
// import scalaz.Id.Id
//
// trait SLensAlg[P[_], L[_], R[_], A, B] extends raw.SLensAlg[P, B]
//     with SOpticAlg[P, L, R, A, B, MonadState, Id] {
//
//   def getL: P[A] = homL[A](evL.get)
//
//   def setL(a: A): P[Unit] = homL(evL.put(a))
//
//   def getR: P[B] = homR[B](evR.get)
//
//   def setR(b: B): P[Unit] = homR(evR.put(b))
// }
//
// object SLensAlg {
//
//   def apply[P[_], L[_], R[_], A, B](
//       homL2: L ~> P,
//       homR2: R ~> P)(implicit
//       ev0: Monad[P],
//       ev1: MonadState[L, A],
//       ev2: MonadState[R, B]) = new SLensAlg[P, L, R, A, B] {
//     def point[X](x: => X) = ev0.point(x)
//     def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
//     implicit val evL = ev1
//     implicit val evR = ev2
//     val homL = homL2
//     val homR = homR2
//   }
// }
