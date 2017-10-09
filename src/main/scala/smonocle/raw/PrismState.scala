package org.hablapps.stateless
package smonocle
package raw

import scalaz.{ Monad, StateT }
import scalaz.syntax.monad._

import monocle.{ Prism => MPrism }

import core.raw.PrismAlg

trait PrismState {

  def asPrismAlg[F[_]: Monad, S, A](
      pr: MPrism[S, A]) = new PrismAlg[StateT[F, S, ?], A] {
    val M = Monad[StateT[F, S, ?]]
    def getOption = StateT(s => (s, pr.getOption(s)).point[F])
    def set(a: A) = StateT(_ => (pr.reverseGet(a), ()).point[F])
  }
}
