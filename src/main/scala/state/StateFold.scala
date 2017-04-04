package org.hablapps.phoropter
package state

import scalaz.{ Reader, ~> }
import scalaz.syntax.functor._
import scalaz.std.tuple._

import monocle.Fold

import core.MonadFold

trait StateFold {

  def fromFold[S, A](fl: Fold[S, A]): MonadFold[Reader[S, ?], Reader[A, ?], A] = {
    new MonadFold[Reader[S, ?], Reader[A, ?], A] {

      def point[X](x: => X) = readerMonad.point(x)

      def bind[X, Y](fx: Reader[S, X])(f: X => Reader[S, Y]) =
        readerMonad.bind(fx)(f)

      implicit val MR = readerMonad

      val hom: Reader[A, ?] ~> λ[x => Reader[S, List[x]]] =
        λ[Reader[A, ?] ~> λ[x => Reader[S, List[x]]]] {
          ra => Reader(s => fl.getAll(s).map(ra.run))
        }
    }
  }
}
