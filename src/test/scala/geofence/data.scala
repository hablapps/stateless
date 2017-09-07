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
  val counterLn: LensAlg[P, GID]

  def fromRegion(reg: Region): G
  def empty: S
}

object System {

  type Aux[S, P2[_], G2, T2] = System[S] {
    type P[X] = P2[X]
    type G = G2
    type T = T2
  }

  type WithP[S, P2[_]] = System[S] { type P[x] = P2[x] }

  def apply[S, P2[_], G2, GP2[_], T2, TP2[_]](
      geofence2: Geofence.WithP[G2, GP2],
      geofenceMp2: MapAlg.Aux[P2, GP2, GID, G2],
      timer2: Timer.WithP[T2, TP2],
      timerLn2: LensAlg.Aux[P2, TP2, T2],
      counterLn2: LensAlg[P2, GID],
      fromRegion2: Region => G2,
      empty2: S): Aux[S, P2, G2, T2] = new System[S] {
    type P[X] = P2[X]
    type G = G2
    type T = T2
    val geofence = geofence2
    val geofenceMp = geofenceMp2
    val timer = timer2
    val timerLn = timerLn2
    val counterLn = counterLn2
    def fromRegion(reg: Region) = fromRegion2(reg)
    def empty = empty2
  }
}

trait Geofence[G] {
  type P[_]

  val regionLn: LensAlg[P, Region]
  val insideLn: LensAlg[P, Set[DID]]
}

object Geofence {

  type Aux[G, P2[_]] = Geofence[G] { type P[x] = P2[x] }

  type WithP[G, P[_]] = Aux[G, P]

  def apply[G, P2[_]](
      regionLn2: LensAlg[P2, Region],
      insideLn2: LensAlg[P2, Set[DID]]): Aux[G, P2] =
    new Geofence[G] {
      type P[X] = P2[X]
      val regionLn = regionLn2
      val insideLn = insideLn2
    }
}

trait Timer[T] {
  type P[_]

  val currentLn: LensAlg[P, Time]
  val alarmMp: MapAlg[P, (GID, DID), Time]
}

object Timer {

  type Aux[T, P2[_]] = Timer[T] { type P[x] = P2[x] }

  type WithP[T, P[_]] = Aux[T, P]

  def apply[T, P2[_]](
      currentLn2: LensAlg[P2, Time],
      alarmMp2: MapAlg[P2, (GID, DID), Time]): Aux[T, P2] =
    new Timer[T] {
      type P[X] = P2[X]
      val currentLn = currentLn2
      val alarmMp = alarmMp2
    }
}
