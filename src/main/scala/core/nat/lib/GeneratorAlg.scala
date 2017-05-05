package org.hablapps.stateless
package core
package nat
package lib

import scalaz.Monad
import scalaz.syntax.monad._

trait GeneratorAlg[P[_], A] {

  implicit val M: Monad[P]

  def next: P[A]

  def nextN(n: Int): P[List[A]] = {
    if (n <= 0) List.empty[A].point[P]
    else next >>= (a => nextN(n - 1) >>= (as => (a :: as).point[P]))
  }
}
