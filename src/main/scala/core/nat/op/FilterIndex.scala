package org.hablapps.stateless
package core
package nat
package op

import shapeless._

trait FilterIndex[P[_], I, A] {
  type Q[_]
  def apply(p: I => Boolean): ITraversalAlg.Aux[P, Q, I :: HNil, A]
}

object FilterIndex {

  type Aux[P[_], Q2[_], I, A] =
    FilterIndex[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], _Q[_], I, A](f: (I=>Boolean) =>
    ITraversalAlg.Aux[P,_Q,I::HNil,A]) = new FilterIndex[P,I,A]{
    type Q[t] = _Q[t]
    def apply(p: I => Boolean) = f(p)
  }

  trait Syntax {
    def filterIndex[P[_], Q[_], I <: HList, A](
        p: I => Boolean)(implicit
        ev: Aux[P, Q, I, A]): ITraversalAlg.Aux[P, Q, I :: HNil, A] =
      ev(p)
  }

  object syntax extends Syntax
}