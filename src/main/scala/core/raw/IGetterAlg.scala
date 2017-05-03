package org.hablapps.stateless
package core
package raw

import scalaz.Monad
import scalaz.syntax.std.option._

trait IGetterAlg[P[_], I, A] extends Monad[P] {

  def get: P[(I, A)]

  /* derived methods */

  def find(p: ((I, A)) => Boolean): P[Option[(I, A)]] =
    map(get)(ia => if (p(ia)) ia.some else None)

  def exist(p: ((I, A)) => Boolean): P[Boolean] = map(get)(p)
}
