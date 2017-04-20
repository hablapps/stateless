// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Monad, MonadState, ~> }
// import scalaz.syntax.monad._
// import scalaz.std.option._
//
// trait IOptionalAlg[P[_], I, A] extends raw.IOptionalAlg[P, A]
//     with IOpticAlg[P, Q, I, A, MonadState, Option] {
//
//   def getOption: P[Option[(I, A)]] = hom(ev.get.strengthL)
//
//   def setOption(a: A): P[Option[Unit]] = hom(_ => ev.put(a))
//
//   /* composing algebras */
//
//   def composeIFold[R[_], J, B](fl: IFoldAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
//     asIFold.composeIFold(fl)
//
//   def composeIGetter[R[_], J, B](gt: IGetterAlg[Q, R, J, B]): IFoldAlg[P, R, (I, J), B] =
//     asIFold.composeIFold(gt.asIFold)
//
//   def composeISetter[R[_], J, B](st: ISetterAlg[Q, R, J, B]): ISetterAlg[P, R, (I, J), B] =
//     asISetter.composeISetter(st)
//
//   def composeITraversal[R[_], J, B](tr: ITraversalAlg[Q, R, J, B]): ITraversalAlg[P, R, (I, J), B] =
//     asITraversal.composeITraversal(tr)
//
//   def composeIOptional[R[_], J, B](op: IOptionalAlg[Q, R, J, B]): IOptionalAlg[P, R, (I, J), B] =
//     IOptionalAlg(λ[λ[x => ((I, J)) => R[x]] ~> λ[x => P[Option[x]]]] { iqx =>
//       map(hom(i => op.hom(j => iqx((i, j)))))(_.join)
//     })(this, op.ev)
//
//   def composeIPrism[R[_], J, B](pr: IPrismAlg[Q, R, J, B]): IOptionalAlg[P, R, (I, J), B] =
//     composeIOptional(pr.asIOptional)
//
//   def composeILens[R[_], J, B](ln: ILensAlg[Q, R, J, B]): IOptionalAlg[P, R, (I, J), B] =
//     composeIOptional(ln.asIOptional)
//
//   /* transforming algebras */
//
//   def asITraversal: ITraversalAlg[P, Q, I, A] =
//     ITraversalAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[List[x]]]] { qx =>
//       map(hom(qx))(_.toList)
//     })(this, ev)
//
//   def asISetter: ISetterAlg[P, Q, I, A] = asITraversal.asISetter
//
//   def asIFold: IFoldAlg[P, Q, I, A] = asITraversal.asIFold
// }
//
// object IOptionalAlg {
//
//   def apply[P[_], Q[_], I, A](
//       hom2: λ[x => I => Q[x]] ~> λ[y => P[Option[y]]])(implicit
//       ev0: Monad[P],
//       ev1: MonadState[Q, A]) = new IOptionalAlg[P, Q, I, A] {
//     def point[X](x: => X) = ev0.point(x)
//     def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
//     implicit val ev = ev1
//     val hom = hom2
//   }
// }
