package org.hablapps.stateless
package smonocle
package raw

import scalaz._, Scalaz._

import monocle.{ Traversal => MTraversal }

import core.raw.TraversalAlg

trait TraversalState {

  def fromTraverse[F[_]: Monad, T[_]: Traverse, A]
      : TraversalAlg[StateT[F, T[A], ?], A] =
    new TraversalAlg[StateT[F, T[A], ?], A] {
      private val M = Monad[StateT[F, T[A], ?]]
      def point[X](x: => X): StateT[F, T[A], X] = M.point(x)
      def bind[X, Y](
          sx: StateT[F, T[A], X])(
          f: X => StateT[F, T[A], Y]): StateT[F, T[A], Y] =
        M.bind(sx)(f)
      def getList: StateT[F, T[A], List[A]] =
        StateT(ta => (ta, ta.foldMap(a => List(a))).point[F])
      def modifyList(f: A => A): StateT[F, T[A], List[Unit]] =
        StateT(ta => (ta.map(f), ta.foldMap(_ => List(()))).point[F])
    }
}
