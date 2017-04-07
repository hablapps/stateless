package org.hablapps.phoropter
package core
package indexed

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.Id.Id
import scalaz.syntax.functor._

trait IGetterAlg[P[_], Q[_], I, A] extends IOpticAlg[P, Q, I, A, MonadReader, Id] {

  def get: P[(I, A)] = hom(ev.ask.strengthL)
}
