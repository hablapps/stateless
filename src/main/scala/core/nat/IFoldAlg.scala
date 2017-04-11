package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.syntax.functor._

trait IFoldAlg[P[_], Q[_], I, A] extends raw.IFoldAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadState, List] {

  def getList: P[List[(I, A)]] = hom(ev.get.strengthL)
}
