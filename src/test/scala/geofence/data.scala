package org.hablapps.stateless
package test
package geofence

import scalaz.State

import core.nat._, lib.MapAlg
import smonocle.nat.all._

trait System[S] {
  type P[_]
  type G
  type T

  val geofence: Geofence[G]
  val geofenceMp: MapAlg.Aux[P, geofence.P, GID, G]
  val timer: Timer[T]
  val timerLn: LensAlg.Aux[P, timer.P, T]
  val counterLn: LensField[P, GID]

  def fromRegion(reg: Region): G
}

object System {
  type Aux[S, P2[_]] = System[S] { type P[x] = P2[x] }
}

trait Geofence[G] {
  type P[_]

  val regionLn: LensField[P, Region]
  val insideLn: LensField[P, Set[DID]]
}

trait Timer[T] {
  type P[_]

  val currentLn: LensField[P, Time]
  val alarmMp: MapAlg.Aux[P, State[Time, ?], (GID, DID), Time]
}
