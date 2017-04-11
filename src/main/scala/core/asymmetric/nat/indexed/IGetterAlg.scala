package org.hablapps.phoropter
package core
package asymmetric
package nat
package indexed

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.Id.Id
import scalaz.syntax.functor._

trait IGetterAlg[P[_], Q[_], I, A] extends raw.indexed.IGetterAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadReader, Id] {

  def get: P[(I, A)] = hom(ev.ask.strengthL)
}
