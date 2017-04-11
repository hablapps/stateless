package org.hablapps.stateless
package core
package nat
package op

trait At[P[_], Q[_], I, A] {
  def at(i: I): LensAlg[P, Q, Option[A]]
}

object At {

  trait Syntax {
    def at[P[_], Q[_], I, A](
        i: I)(implicit
        ev: At[P, Q, I, A]): LensAlg[P, Q, Option[A]] =
      ev.at(i)
  }

  object syntax extends Syntax
}
