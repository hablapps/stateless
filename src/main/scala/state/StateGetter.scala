package org.hablapps.phoropter
package state

import scalaz.{ Reader, ~> }
import scalaz.syntax.functor._
import scalaz.std.tuple._

import monocle.Getter

import core.MonadGetter

trait StateGetter {

  def fromGetter[S, A](gt: Getter[S, A]): MonadGetter[Reader[S, ?], Reader[A, ?], A] = {
    new MonadGetter[Reader[S, ?], Reader[A, ?], A] {

      def point[X](x: => X) = readerMonad.point(x)

      def bind[X, Y](fx: Reader[S, X])(f: X => Reader[S, Y]) =
        readerMonad.bind(fx)(f)

      // TODO: dummy implementation
      def local[X](f: A => A)(fx: Reader[S, X]): Reader[S, X] = fx

      implicit val MR = readerMonad

      val hom: Reader[A, ?] ~> Reader[S, ?] = λ[Reader[A, ?] ~> Reader[S, ?]] {
        ra => Reader(s => ra.run(gt.get(s)))
      }
    }
  }
}
