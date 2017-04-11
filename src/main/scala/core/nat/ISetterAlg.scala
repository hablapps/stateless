package org.hablapps.stateless
package core
package asymmetric
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.Id.Id

trait ISetterAlg[P[_], Q[_], I, A] extends raw.ISetterAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadState, Id] {

  def modify(f: A => A): P[Unit] = hom(_ => ev.modify(f))
}
