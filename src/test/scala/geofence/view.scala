package org.hablapps.stateless
package test
package geofence

import Function.const

import scalaz._, Scalaz._

import shapeless._

trait GeofenceView[P[_]] {

  def add(reg: Region): P[GID]

  def remove(gid: GID): P[Unit]

  def at(did: DID, pos: Position): P[List[(GID, OutputEvent)]]

  def tick(time: Time): P[List[((GID, DID), Time)]]

  def destroy: P[Unit]
}

object GeofenceView {

  def fromData[S, P[_]](
      sys: System.Aux[S, P])(implicit
      ev0: Monad[P]): GeofenceView[P] = new GeofenceView[sys.P] {
    import sys.{ P => _, _ }
    import timer.{ P => _, _ }

    def add(reg: Region): P[GID] =
      for {
        gid <- counterLn.get
        _   <- counterLn.modify(_ + 1)
        _   <- geofenceMp(gid).set(Option(fromRegion(reg)))
      } yield gid

    def remove(gid: GID): P[Unit] =
      for {
        _  <- geofenceMp(gid).set(None)
        tr = timerLn.asIndexed.composeTraversal(alarmMp.filterIndex(_._1 == gid))
        ks <- tr.indexes
        _  <- ks.traverse(k => timerLn.composeLens(alarmMp(k.head)).set(None))
      } yield ()

    def at(did: DID, pos: Position): P[List[(GID, OutputEvent)]] = ???

    def tick(time: Time): P[List[((GID, DID), Time)]] =
      for {
        _   <- timerLn.composeLens(currentLn).set(time)
        off <- timerLn.asIndexed.composeTraversal(alarmMp).foldMap {
          case (i, t) => if (t <= time) List.empty else List((i.head, t))
        }
      } yield off

    def destroy: P[Unit] =
      for {
        _ <- geofenceMp.destroy
        _ <- timerLn.hom(alarmMp.destroy)
      } yield ()
  }
}
