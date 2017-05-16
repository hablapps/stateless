package org.hablapps.stateless
package smonocle
package raw

import scalaz._, Scalaz._

import core.raw.FoldAlg

trait FoldState {

  def fromFunctor[F[_]: Monad, T[_]: Foldable, A]
      : FoldAlg[ReaderT[F, T[A], ?], A] =
    new FoldAlg[ReaderT[F, T[A], ?], A] {
      private val M = Monad[ReaderT[F, T[A], ?]]
      def point[X](x: => X): ReaderT[F, T[A], X] = M.point(x)
      def bind[X, Y](
          sx: ReaderT[F, T[A], X])(
          f: X => ReaderT[F, T[A], Y]): ReaderT[F, T[A], Y] =
        M.bind(sx)(f)
      def getList: ReaderT[F, T[A], List[A]] =
        ReaderT(ta => ta.foldMap(a => List(a)).point[F])
    }
}
