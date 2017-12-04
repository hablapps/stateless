package org.hablapps.stateless
package core
package nat
package op

trait At[P[_], I, A] {
  type Q[_]
  def at(i: I): LensAlg.Aux[P, Q, Option[A]]
}

object At {

  type Aux[P[_], Q2[_], I, A] = At[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], _Q[_], I, A](index: I => LensAlg.Aux[P, _Q, Option[A]]) =
    new At[P,I,A]{
      type Q[t] = _Q[t]
      def at(i: I): LensAlg.Aux[P,Q,Option[A]] =
        index(i)
    }

  trait Syntax {
    def at[P[_], Q[_], I, A](
        i: I)(implicit
        ev: At.Aux[P, Q, I, A]): LensAlg.Aux[P, Q, Option[A]] =
      ev.at(i)
  }

  object syntax extends Syntax
}