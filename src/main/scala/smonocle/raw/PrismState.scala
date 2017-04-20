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
    private val M = Monad[StateT[F, S, ?]]
    def point[X](x: => X) = M.point(x)
    def bind[X, Y](fx: StateT[F, S, X])(f: X => StateT[F, S, Y]) = M.bind(fx)(f)
    def getOption = StateT(s => (s, pr.getOption(s)).point[F])
    def set(a: A) = StateT(_ => (pr.reverseGet(a), ()).point[F])
  }
}
