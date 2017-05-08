package org.hablapps.stateless
package core
package nat
package lib

import Function.const

import scalaz.{ Monad, MonadState }
import scalaz.syntax.monad._
import scalaz.syntax.std.option._

import shapeless._

import op.{ At, FilterIndex }

trait MapAlg[P[_], K, V] {

  type Q[_]

  val ev0: At[P, K, V]

  val ev1: FilterIndex.Aux[P, Q, K, V]

  implicit val M: Monad[P]

  /* derived methods */

  def apply(k: K): LensAlg[P, Option[V]] = ev0.at(k)

  def pick[O](k: K)(qo: Q[O]): P[Option[O]] =
    ev1.filterIndex(_ == k).hom(_ => qo).map(_.headOption)
}

object MapAlg {

  type Aux[P[_], Q2[_], K, V] = MapAlg[P, K, V] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], K, V](
      ev02: At[P, K, V],
      ev12: FilterIndex.Aux[P, Q2, K, V])(implicit
      M2: Monad[P]): Aux[P, Q2, K, V] = new MapAlg[P, K, V] {
    type Q[x] = Q2[x]
    val ev0 = ev02
    val ev1 = ev12
    val M = M2
  }

  implicit def toTraversal[P[_], K, V](
      map: MapAlg[P, K, V]): ITraversalAlg.Aux[P, map.Q, K :: HNil, V] =
    map.ev1.filterIndex(const(true))
}
