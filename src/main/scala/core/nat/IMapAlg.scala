// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Monad, MonadState, ~> }
// import scalaz.syntax.functor._
// import scalaz.syntax.id._
//
// import op._, At.syntax._
//
// trait IMapAlg[P[_], R[_], I, A] extends raw.IMapAlg[P, A]
//     with IOpticAlg[P, Q, I, A, MonadState, List] {
//
//   implicit val ta: At[P, R, I, A]
//
//   /* derived algebra */
//
//   def getList: P[List[(I, A)]] = hom(ev.get.strengthL)
//
//   def modifyList(f: A => A): P[List[Unit]] = hom(_ => ev.modify(f))
//
//   def updateOption(i: I)(oa: Option[A]): P[Unit] =
//     at(i) |> (ln => ln.hom(ln.ev.put(oa)))
//
//   /* new algebra */
//
//   def collect[O](qo: Q[O]): P[List[O]] = hom(_ => qo)
//
//   def pick[O](i: I)(ro: R[O]): P[O] = at(i).hom[O](ro)
// }
//
// object IMapAlg {
//
//   def apply[P[_], Q[_], R[_], I, A](
//       hom2: λ[x => I => Q[x]] ~> λ[x => P[List[x]]])(implicit
//       ev0: Monad[P],
//       ev1: MonadState[Q, A],
//       ev2: At[P, R, I, A]) = new IMapAlg[P, Q, R, I, A] {
//     def point[X](x: => X) = ev0.point(x)
//     def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
//     implicit val ev = ev1
//     val hom = hom2
//     implicit val ta = ev2
//   }
// }
