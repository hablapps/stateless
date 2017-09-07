package org.hablapps.stateless
package test
package geofence

import org.scalatest._

import monocle.macros.Lenses
import monocle.function.all._

import scalaz._, Scalaz._

import core.nat.lib.MapAlg
import smonocle.nat.all._

import org.hablapps.puretest, puretest._, puretest.Filter.Syntax._

class StateTests extends FunSpec with Matchers with MyTest[SNetwork, SNetwork.Program] {

  implicit val MonadErrorP = stateTMonadError[SNetwork, Throwable \/ ?, Throwable]

  lazy val sys: System.WithP[SNetwork, SNetwork.Program] = SNetwork.model

  val FilterP: puretest.Filter[SNetwork.Program] = puretest.Filter[SNetwork.Program]

  val Tester: StateTester[SNetwork.Program, SNetwork, Throwable] =
    StateTester.StateTStateTester[Throwable, Throwable \/ ?, SNetwork]

  // XXX: this is sooo weird!
  implicit val Monad2: Monad[sys.geofence.P] = sys.geofence.regionLn
  implicit val Monad3: Monad[sys.timer.P] = sys.timer.currentLn
}

@Lenses case class SNetwork(n: GID, geofences: Map[GID, SGeofence], alarms: STimer)

@Lenses case class SGeofence(region: Region, in: Set[DID] = Set.empty[DID])

@Lenses case class STimer(alarms: Map[(GID, DID), Time], current: Time)

object SNetwork {

  type Program[A]  = StateT[Throwable \/ ?, SNetwork, A]

  def model: System.Aux[SNetwork, Program, SGeofence, STimer] =
    System[SNetwork, Program, SGeofence, SGeofence.Program, STimer, STimer.Program](
      SGeofence.model,
      mapFromMapMLens[Throwable \/ ?, SNetwork, GID, SGeofence](SNetwork.geofences),
      STimer.model,
      asLensAlg[Throwable \/ ?, SNetwork, STimer](SNetwork.alarms),
      asLensField[Throwable \/ ?, SNetwork, Int](SNetwork.n),
      SGeofence(_),
      SNetwork(0, Map.empty, STimer(Map.empty, 0)))
}

object SGeofence {

  type Program[A] = StateT[Throwable \/ ?, SGeofence, A]

  def model: Geofence.Aux[SGeofence, Program] =
    Geofence[SGeofence, Program](
      asLensField[Throwable \/ ?, SGeofence, Region](SGeofence.region),
      asLensField[Throwable \/ ?, SGeofence, Set[DID]](SGeofence.in))
}

object STimer {

  type Program[A] = StateT[Throwable \/ ?, STimer, A]

  def model: Timer.Aux[STimer, Program] =
    Timer[STimer, Program](
      asLensField[Throwable \/ ?, STimer, Time](STimer.current),
      mapFromMapMLens[Throwable \/ ?, STimer, (GID, DID), Time](STimer.alarms))
}
