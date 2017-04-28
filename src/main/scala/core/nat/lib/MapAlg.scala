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

  val tr: ITraversalAlg.Aux[P, Q, K :: HNil, V] // TODO: extends ITraversalAlg

  val ev: At[P, K, V]

  implicit val M: Monad[P]

  /* derived methods */

  implicit lazy val MS: MonadState[tr.Q, V] = tr.ev

  def apply(k: K): LensAlg[P, Option[V]] = ev.at(k)

  def getList: P[List[(K, V)]] = tr.hom(k => tr.ev.get.strengthL(k.head))

  def modifyList(f: V => V): P[List[Unit]] = tr.hom(_ => tr.ev.modify(f))

  def updateOption(k: K)(ov: Option[V]): P[Unit] = apply(k).set(ov)

  def modify(f: V => V): P[Unit] = modifyList(f).void

  def setList(v: V): P[List[Unit]] = modifyList(_ => v)

  def set(v: V): P[Unit] = setList(v).void

  def indexes: P[List[K]] = getList.map(_.unzip._1)

  def foci: P[List[V]] = getList.map(_.unzip._2)

  def add(k: K)(v: V): P[Unit] = updateOption(k)(v.some)

  def remove(k: K): P[Unit] = updateOption(k)(None)

  def get(k: K): P[Option[V]] = getList.map(_.toMap.get(k))
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
}
