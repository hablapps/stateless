package org.hablapps.stateless
package core
package nat
package lib

import scalaz.{ Monad, MonadState }
import scalaz.syntax.monad._
import scalaz.syntax.std.option._

import shapeless._

import op.At

trait MapAlg[P[_], K, V] {

  type Q[_]

  val tr: ITraversalAlg.Aux[P, Q, K :: HNil, V]

  val ev: At[P, K, V]

  implicit val M: Monad[P]

  /* derived methods */

  implicit lazy val MS: MonadState[tr.Q, V] = tr.ev

  def apply(k: K): LensAlg[P, Option[V]] = ev.at(k)

  def add(k: K)(v: V): P[Unit] = apply(k).set(v.some)

  def remove(k: K): P[Unit] = apply(k).set(None)
}

object MapAlg {

  type Aux[P[_], Q2[_], K, V] = MapAlg[P, K, V] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], K, V](
      tr2: ITraversalAlg.Aux[P, Q2, K :: HNil, V],
      ev2: At[P, K, V])(implicit
      M2: Monad[P]): Aux[P, Q2, K, V] = new MapAlg[P, K, V] {
    type Q[x] = Q2[x]
    val tr = tr2
    val ev = ev2
    val M = M2
  }

  implicit def toTraversal[P[_], K, V](
      map: MapAlg[P, K, V]): ITraversalAlg.Aux[P, map.Q, K :: HNil, V] =
    map.tr
}
