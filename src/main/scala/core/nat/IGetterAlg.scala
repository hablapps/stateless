// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Monad, MonadReader, ~> }
// import scalaz.Id.Id
// import scalaz.syntax.functor._
//
// trait IGetterAlg[P[_], I, A] extends raw.IGetterAlg[P, A]
//     with IOpticAlg[P, Q, I, A, MonadReader, Id] {
//
//   def get: P[(I, A)] = hom(ev.ask.strengthL)
//
//   /* composing algebras */
//
//   def composeIFold[R[_], J, B](fl: IFoldAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
//     asIFold.composeIFold(fl)
//
//   def composeIGetter[R[_], J, B](gt: IGetterAlg[Q, R, J, B]): IGetterAlg[P, R, (I, J), B] =
//     IGetterAlg(new (λ[x => ((I, J)) => R[x]] ~> P) {
//       def apply[X](iqx: ((I, J)) => R[X]): P[X] =
//         hom[X](i => gt.hom[X](j => iqx((i, j))))
//     })(this, gt.ev)
//
//   def composeITraversal[R[_], J, B](tr: ITraversalAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
//     composeIFold(tr.asIFold)
//
//   def composeIOptional[R[_], J, B](op: IOptionalAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
//     composeIFold(op.asIFold)
//
//   def composeIPrism[R[_], J, B](pr: IPrismAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
//     composeIFold(pr.asIFold)
//
//   def composeILens[R[_], J, B](ln: ILensAlg[Q, R, J, B]): IGetterAlg[P, R, (I, J), B] =
//     composeIGetter(ln.asIGetter)
//
//   /* transforming algebras */
//
//   def asIFold: IFoldAlg[P, Q, I, A] =
//     IFoldAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[List[x]]]] { qx =>
//       map(hom(qx))(List(_))
//     })(this, ev)
// }
//
// object IGetterAlg {
//
//   def apply[P[_], Q[_], I, A](
//       hom2: λ[x => I => Q[x]] ~> P)(implicit
//       ev0: Monad[P],
//       ev1: MonadReader[Q, A]) = new IGetterAlg[P, Q, I, A] {
//     def point[X](x: => X) = ev0.point(x)
//     def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
//     implicit val ev = ev1
//     val hom = hom2
//   }
// }
