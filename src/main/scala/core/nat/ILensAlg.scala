// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Monad, MonadState, ~> }
// import scalaz.Id.Id
// import scalaz.syntax.functor._
// import scalaz.syntax.std.option._
//
// trait ILensAlg[P[_], I, A] extends raw.ILensAlg[P, A]
//     with IOpticAlg[P, Q, I, A, MonadState, Id] {
//
//   def get: P[(I, A)] = hom(ev.get.strengthL)
//
//   def set(a: A): P[Unit] = hom(_ => ev.put(a))
//
//   /* composing algebras */
//
//   def composeIFold[R[_], J, B](fl: IFoldAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
//     asIFold.composeIFold(fl)
//
//   def composeIGetter[R[_], J, B](gt: IGetterAlg[Q, R, J, B]): IGetterAlg[P, R, (I, J), B] =
//     asIGetter.composeIGetter(gt)
//
//   def composeISetter[R[_], J, B](st: ISetterAlg[Q, R, J, B]): ISetterAlg[P, R, (I, J), B] =
//     asISetter.composeISetter(st)
//
//   def composeITraversal[R[_], J, B](tr: ITraversalAlg[Q, R, J, B]): ITraversalAlg[P, R, (I, J), B] =
//     asITraversal.composeITraversal(tr)
//
//   def composeIOptional[R[_], J, B](op: IOptionalAlg[Q, R, J, B]): IOptionalAlg[P, R, (I, J), B] =
//     asIOptional.composeIOptional(op)
//
//   def composeIPrism[R[_], J, B](pr: IPrismAlg[Q, R, J, B]): IOptionalAlg[P, R, (I, J), B] =
//     asIOptional.composeIOptional(pr.asIOptional)
//
//   def composeILens[R[_], J, B](ln: ILensAlg[Q, R, J, B]): ILensAlg[P, R, (I, J), B] =
//     ILensAlg(new (λ[x => ((I, J)) => R[x]] ~> P) {
//       def apply[X](iqx: ((I, J)) => R[X]): P[X] =
//         hom[X](i => ln.hom[X](j => iqx((i, j))))
//     })(this, ln.ev)
//
//   /* transforming algebras */
//
//   def asIGetter: IGetterAlg[P, Q, I, A] = IGetterAlg(hom)(this, ev)
//
//   def asIOptional: IOptionalAlg[P, Q, I, A] =
//     IOptionalAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[Option[x]]]](
//       qx => map(hom(qx))(_.some)))(this, ev)
//
//   def asIFold: IFoldAlg[P, Q, I, A] = asIGetter.asIFold
//
//   def asITraversal: ITraversalAlg[P, Q, I, A] = asIOptional.asITraversal
//
//   def asISetter: ISetterAlg[P, Q, I, A] = asITraversal.asISetter
// }
//
// object ILensAlg {
//
//   def apply[P[_], Q[_], I, A](
//       hom2: λ[x => I => Q[x]] ~> P)(implicit
//       ev0: Monad[P],
//       ev1: MonadState[Q, A]) = new ILensAlg[P, Q, I, A] {
//     def point[X](x: => X) = ev0.point(x)
//     def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
//     implicit val ev = ev1
//     val hom = hom2
//   }
// }
