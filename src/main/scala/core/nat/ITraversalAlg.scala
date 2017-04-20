// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Const, Monad, MonadState, ~> }
// import scalaz.syntax.monad._
// import scalaz.std.list._
//
// trait ITraversalAlg[P[_], I, A] extends raw.ITraversalAlg[P, A]
//     with IOpticAlg[P, Q, I, A, MonadState, List] {
//
//   def getList: P[List[(I, A)]] = hom(ev.get.strengthL)
//
//   def modifyList(f: A => A): P[List[Unit]] = hom(_ => ev.modify(f))
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
//     ITraversalAlg(λ[λ[x => ((I, J)) => R[x]] ~> λ[x => P[List[x]]]] { iqx =>
//       map(hom(i => tr.hom(j => iqx((i, j)))))(_.join)
//     })(this, tr.ev)
//
//   def composeIOptional[R[_], J, B](op: IOptionalAlg[Q, R, J, B]): ITraversalAlg[P, R, (I, J), B] =
//     composeITraversal(op.asITraversal)
//
//   def composeIPrism[R[_], J, B](pr: IPrismAlg[Q, R, J, B]): ITraversalAlg[P, R, (I, J), B] =
//     composeITraversal(pr.asITraversal)
//
//   def composeILens[R[_], J, B](ln: ILensAlg[Q, R, J, B]): ITraversalAlg[P, R, (I, J), B] =
//     composeITraversal(ln.asITraversal)
//
//   /* transforming algebras */
//
//   def asIFold: IFoldAlg[P, Q, I, A] = IFoldAlg(hom)(this, ev)
//
//   def asISetter: ISetterAlg[P, Q, I, A] =
//     ISetterAlg(λ[λ[x => I => Q[x]] ~> λ[x => P[Const[Unit, x]]]] { qx =>
//       map(hom(qx))(_ => Const(()))
//     })(this, ev)
// }
//
// object ITraversalAlg {
//
//   def apply[P[_], Q[_], I, A](
//       hom2: λ[x => I => Q[x]] ~> λ[x => P[List[x]]])(implicit
//       ev0: Monad[P],
//       ev1: MonadState[Q, A]) = new ITraversalAlg[P, Q, I, A] {
//     def point[X](x: => X) = ev0.point(x)
//     def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
//     implicit val ev = ev1
//     val hom = hom2
//   }
// }
