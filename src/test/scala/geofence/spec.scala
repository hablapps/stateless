package org.hablapps.stateless
package test
package geofence

import scalaz.MonadError
import scalaz.syntax.monad._

import org.hablapps.puretest._, Filter.Syntax._

trait GeofenceSpec[P[_]] {

  /* To be tested */

  val view: GeofenceView[P]

  /* Testing Dependencies */

  implicit val MonadErrorP: MonadError[P, Throwable]
  implicit val FilterP: Filter[P]

  /* Tests */

  import view._

  def prefix: P[(GID, GID)] =
    for {
      gid1 <- add((5, (0, 0)))
      gid2 <- add((10, (0, 0)))
      _    <- tick(0)
    } yield (gid1, gid2)

  // Test 1: device event

  def test1: P[Unit] =
    for {
      (gid1, gid2) <- prefix
      List((gid1, Enter(33)), (gid2, Enter(33))) <- at(33, (1, 1))
    } yield ()
}
