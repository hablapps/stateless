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

  trait Syntax {
    def at[P[_], Q[_], I, A](
        i: I)(implicit
        ev: At.Aux[P, Q, I, A]): LensAlg.Aux[P, Q, Option[A]] =
      ev.at(i)
  }

  object syntax extends Syntax
}
