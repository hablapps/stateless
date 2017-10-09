package org.hablapps.stateless
package core
package nat
package lib

import Function.const

import scalaz._, Scalaz._

import shapeless._

import op.{ At, FilterIndex }

trait MapAlg[P[_], K, V] {

  type Q[_]

  val ev0: At[P, K, V]

  val filterIndex: FilterIndex.Aux[P, Q, K, V]

  implicit val M: Monad[P]

  /* derived methods */

  def apply(k: K): LensAlg[P, Option[V]] = ev0.at(k)

  def pick(k: K): ITraversalAlg.Aux[P, Q, K :: HNil, V] =
    filterIndex(_ == k)

  def pick[O](k: K, qo: Q[O]): P[Option[O]] =
    pick(k).hom(_ => qo).map(_.headOption)

  // def destroy: P[Unit] =
  //   for {
  //     ks <- filterIndex(const(true)).indexes
  //     _  <- ks.traverse(k => ev0.at(k.head).set(None))
  //   } yield ()
}

object MapAlg {

  type Aux[P[_], Q2[_], K, V] = MapAlg[P, K, V] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], K, V](
      ev02: At[P, K, V],
      filterIndex2: FilterIndex.Aux[P, Q2, K, V])(implicit
      M2: Monad[P]): Aux[P, Q2, K, V] = new MapAlg[P, K, V] {
    type Q[x] = Q2[x]
    val ev0 = ev02
    val filterIndex = filterIndex2
    val M = M2
  }

  implicit def toTraversal[P[_], K, V](
      map: MapAlg[P, K, V]): ITraversalAlg.Aux[P, map.Q, K :: HNil, V] =
    map.filterIndex(const(true))
}
