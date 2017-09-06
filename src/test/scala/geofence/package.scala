package org.hablapps.stateless
package test

package object `geofence` {

  type GID = Int // geofence id
  type DID = Int // device id
  type Position = (Int, Int)
  type Time = Int
  type Radius = Int
  type Region = (Radius, Position)

  sealed abstract class OutputEvent
  case class Enter(did: DID) extends OutputEvent
  case class Exit(did: DID)  extends OutputEvent
}
