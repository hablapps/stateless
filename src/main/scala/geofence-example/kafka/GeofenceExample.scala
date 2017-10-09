package org.hablapps.stateless
package kafka
package nat

import java.util.Properties
import org.apache.kafka.clients.producer.{Producer, KafkaProducer}
import org.apache.kafka.clients.consumer.KafkaConsumer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.StateT
import scalaz.std.scalaFuture._

import core.nat.GeofenceExample.SGeofence
import geofence._, Programs._

object GeofenceExample {

  type S = (Producer[Unit, String], SGeofence)
  type P[A] = StateT[Future, S, A]

  implicit val MS = StateT.stateTMonadState[S, Future]

  val geo: Geofence[P] = GenKafka.stateT(
    writer.nat.GeofenceExample.iso,
    writer.nat.GeofenceExample.geo)

}

object GeofenceExampleRun extends App {
  import GeofenceExample._

  val propsP: Properties = new Properties()
  propsP.put("bootstrap.servers", "localhost:9092")
  propsP.put("acks", "all")
  propsP.put("retries", "0")
  propsP.put("batch.size", "16384")
  propsP.put("linger.ms", "1")
  propsP.put("buffer.memory", "33554432")
  propsP.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer")
  propsP.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer")

  val producer = new KafkaProducer[Unit, String](propsP)

  val propsC: Properties = new Properties()
  propsC.put("bootstrap.servers", "localhost:9092")
  propsC.put("enable.auto.commit", "true")
  propsC.put("auto.commit.interval.ms", "1000")
  propsC.put("key.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")
  propsC.put("value.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")

  val consumer = new KafkaConsumer[Unit, String](propsC)

  val recoveredState = GenKafka.recover(
    writer.nat.GeofenceExample.iso)(
    writer.nat.GeofenceExample.circeIso,
    core.nat.GeofenceExample.geo)(consumer)(SGeofence(1, Set(2, 3, 4)))

  val prog = progGen2(geo)(StateT.stateTMonadState[S, Future])

  val res = Await.result(prog.eval((producer, recoveredState)), 10 seconds)
  println(s"RES: $res")

}
