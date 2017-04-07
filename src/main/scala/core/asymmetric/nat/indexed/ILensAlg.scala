package org.hablapps.phoropter
package core
package asymmetric
package nat
package indexed

import scalaz.{ Monad, MonadState, ~> }
import scalaz.Id.Id
import scalaz.syntax.functor._

trait ILensAlg[P[_], Q[_], I, A] extends IOpticAlg[P, Q, I, A, MonadState, Id] {

  def get: P[(I, A)] = hom(ev.get.strengthL)

  def put(a: A): P[Unit] = hom(_ => ev.put(a))
}
