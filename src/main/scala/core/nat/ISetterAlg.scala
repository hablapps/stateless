// package org.hablapps.stateless
// package core
// package nat
//
// import scalaz.{ Const, Monad, MonadState, ~> }
//
// trait ISetterAlg[P[_], I, A] extends raw.ISetterAlg[P, A]
//     with IOpticAlg[P, Q, I, A, MonadState, Const[Unit, ?]] {
//
//   def modify(f: A => A): P[Unit] = map(hom(_ => ev.modify(f)))(_.getConst)
//
//   def composeISetter[R[_], J, B](st: ISetterAlg[Q, R, J, B]): ISetterAlg[P, R, (I, J), B] =
//     ISetterAlg(λ[λ[x => ((I, J)) => R[x]] ~> λ[x => P[Const[Unit, x]]]] { iqx =>
//       map(hom(i => st.hom(j => iqx((i, j)))))(_ => Const(()))
//     })(this, st.ev)
//
//   def composeITraversal[R[_], J, B](tr: ITraversalAlg[Q, R, J, B]): ISetterAlg[P, R, (I, J), B] =
//     composeISetter(tr.asISetter)
//
//   def composeIOptional[R[_], J, B](op: IOptionalAlg[Q, R, J, B]): ISetterAlg[P, R, (I, J), B] =
//     composeISetter(op.asISetter)
//
//   def composeIPrism[R[_], J, B](pr: IPrismAlg[Q, R, J, B]): ISetterAlg[P, R, (I, J), B] =
//     composeISetter(pr.asISetter)
//
//   def composeILens[R[_], J, B](ln: ILensAlg[Q, R, J, B]): ISetterAlg[P, R, (I, J), B] =
//     composeISetter(ln.asISetter)
// }
//
// object ISetterAlg {
//
//   def apply[P[_], Q[_], I, A](
//       hom2: λ[x => I => Q[x]] ~> λ[x => P[Const[Unit, x]]])(implicit
//       ev0: Monad[P],
//       ev1: MonadState[Q, A]) = new ISetterAlg[P, Q, I, A] {
//     def point[X](x: => X) = ev0.point(x)
//     def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
//     implicit val ev = ev1
//     val hom = hom2
//   }
// }
