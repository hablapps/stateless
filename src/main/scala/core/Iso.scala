package org.hablapps.phoropter
package core

import scalaz.Isomorphism.<~>

trait Iso[P[_], Q[_]] {
  val iso: P <~> Q
}
