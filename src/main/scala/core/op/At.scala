package org.hablapps.phoropter
package core
package op

trait At[P[_], Q[_], I, A] {
  def at(i: I): MonadLens[P, Q, Option[A]]
}

object At {

  trait Syntax {
    def at[P[_], Q[_], I, A](
        i: I)(implicit
        ev: At[P, Q, I, A]): MonadLens[P, Q, Option[A]] =
      ev.at(i)
  }

  object syntax extends Syntax
}
