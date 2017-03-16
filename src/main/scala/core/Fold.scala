package org.hablapps.phoropter
package core

import scalaz.{ Functor, Monoid }
import scalaz.syntax.functor._
import scalaz.syntax.foldable._
import scalaz.std.list._

trait Fold[P[_], A] {

  def fold: P[List[A]]

  // derived methods

  def foldMap[M: Monoid](f: A => M)(implicit F: Functor[P]): P[M] =
    fold.map(_.foldMap(f))
}
