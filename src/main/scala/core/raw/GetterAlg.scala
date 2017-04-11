package org.hablapps.stateless
package core
package raw

import scalaz.{ Equal, Monad, MonadReader }
import scalaz.syntax.equal._
import scalaz.syntax.monad._

trait GetterAlg[P[_], A] extends MonadReader[P, A] { self =>

  def get: P[A] = ask

  // FIXME: dummy implementation
  def local[X](f: A => A)(px: P[X]) = px

  trait GetterAlgLaw {
    implicit val _: Monad[P] = self

    def getGet(implicit eq: Equal[P[(A, A)]]): Boolean =
      (get >>= (a1 => get >>= (a2 => (a1, a2).point[P]))) ===
        (get >>= (a => (a, a).point[P]))
  }
}
