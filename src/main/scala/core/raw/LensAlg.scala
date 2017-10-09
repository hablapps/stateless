package org.hablapps.stateless
package core
package raw

import scalaz.{ Equal, Monad }
import scalaz.syntax.monad._
import scalaz.syntax.equal._
import scalaz.syntax.std.option._

trait LensAlg[P[_], A] {

  /* EVIDENCES */
  implicit val M: Monad[P]

  def put(a: A): P[Unit]

  def get: P[A]

  /* derived methods */

  def find(p: A => Boolean): P[Option[A]] =
    get map { a => if (p(a)) a.some else None }

  def gets[B](f: A => B): P[B] = get map f

  def exist(p: A => Boolean): P[Boolean] = gets(p)

  def modify(f: A => A): P[Unit] = get >>= (f andThen put)

  trait LensAlgLaw {

    def getGet(implicit eq: Equal[P[(A, A)]]): Boolean =
      (get >>= (a1 => get >>= (a2 => (a1, a2).point[P]))) ===
        (get >>= (a => (a, a).point[P]))

    def getPut(implicit eq: Equal[P[Unit]]): Boolean =
      (get >>= put) === ().point[P]

    def putGet(a: A)(implicit eq: Equal[P[A]]): Boolean =
      (put(a) >> get) === (put(a) >> a.point[P])

    def putPut(a1: A, a2: A)(implicit eq: Equal[P[Unit]]): Boolean =
      (put(a1) >> put(a2)) === put(a2)
  }

  def lensAlgLaw = new LensAlgLaw {}
}
