package org.hablapps.phoropter
package core
package indexed

import scalaz.{ Monad, MonadState, ~> }

trait MonadILens[P[_], Q[_], I, A] extends Monad[P] {

  implicit val MS: MonadState[Q, (I, A)]
  val hom: Q ~> P

  /* derived algebra */

  def get: P[(I, A)] = hom(MS.get)

  def put(a: A): P[Unit] = bind(hom(MS.get)) {
    case (i, _) => hom(MS.put((i, a)))
  }
}
