package org.hablapps.stateless
package smonocle
package raw

import scalaz._, Scalaz._

import core.raw.SetterAlg

trait SetterState {

  def fromFunctor[F[_]: Monad, T[_]: Functor, A]
      : SetterAlg[StateT[F, T[A], ?], A] =
    new SetterAlg[StateT[F, T[A], ?], A] {
      private val M = Monad[StateT[F, T[A], ?]]
      def point[X](x: => X): StateT[F, T[A], X] = M.point(x)
      def bind[X, Y](
          sx: StateT[F, T[A], X])(
          f: X => StateT[F, T[A], Y]): StateT[F, T[A], Y] =
        M.bind(sx)(f)
      def modify(f: A => A): StateT[F, T[A], Unit] =
        StateT(ta => (ta.map(f), ()).point[F])
    }
}
