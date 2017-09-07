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
      _    <- at(33, (1, 1))
    } yield (gid1, gid2)

  // Test 1: existing device stays in geofences

  def test1: P[Unit] =
    for {
      _ <- prefix
      Nil <- at(33, (1, 1))
    } yield ()

  // Test 2: new device enters in geofences

  def test2: P[Unit] =
    for {
      (gid1, gid2) <- prefix
      List((`gid1`, Enter(34)), (`gid2`, Enter(34))) <- at(34, (1, 1))
    } yield ()

  // Test 3: existing device abandons inner geofence

  def test3: P[Unit] =
    for {
      (gid1, gid2) <- prefix
      List((`gid1`, Exit(33))) <- at(33, (6, 6))
    } yield ()

  // Test 4: existing device abandons both geofences

  def test4: P[Unit] =
    for {
      (gid1, gid2) <- prefix
      List((`gid1`, Exit(33)), (`gid2`, Exit(33))) <- at(33, (11, 6))
    } yield ()

  // Test 5: remove geofences affects further moves

  def test5: P[Unit] =
    for {
      (gid1, gid2) <- prefix
      _ <- remove(gid1)
      _ <- remove(gid2)
      Nil <- at(33, (11, 6))
    } yield ()

  // Test 6: went off alarms are notified

  def test6: P[Unit] =
    for {
      (gid1, gid2) <- prefix
      List(((`gid1`, 33), 3), ((`gid2`, 33), 3)) <- tick(3)
    } yield ()

  // Test 7: went off alarms are removed when abandoning geofence

  def test7: P[Unit] =
    for {
      (gid1, gid2) <- prefix
      _ <- at(33, (6, 6))
      List(((`gid2`, 33), 3)) <- tick(3)
    } yield ()
}
