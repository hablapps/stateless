package org.hablapps.stateless
package kafka
package nat

import io.circe.parser._
import org.apache.kafka.clients.producer.{Producer, ProducerRecord}
import org.apache.kafka.clients.consumer.Consumer
import org.apache.kafka.common.TopicPartition
import scala.concurrent.{blocking, Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scalaz._
import scalaz.std.list._
import scalaz.std.scalaFuture._
import scalaz.syntax.id._

import writer.nat.GenWriter.ButtonPressed

object GenKafka {

  def stateT[TC[_[_]], S](
      iso: core.Iso[TC],
      internal: TC[StateT[Writer[List[ButtonPressed], ?], S, ?]])(implicit
      CQRS: core.CQRS[iso.ADT],
      Ev: iso.Ev[StateT[Future, (Producer[Unit, String], S), ?]],
      EC: ExecutionContext): TC[StateT[Future, (Producer[Unit, String], S), ?]] = {

    import iso._

    type Env    = (Producer[Unit, String], S)
    type W  [X] = Writer[List[ButtonPressed], X]
    type P  [X] = StateT[     W,   S, X]
    type Aux[X] = StateT[     W, Env, X]
    type Q  [X] = StateT[Future, Env, X]

    val pToQ = new (Aux ~> Q) {
      def apply[B](pb: Aux[B]): Q[B] = StateT[Future, Env, B] { case env =>
        val (w, ((p, s), o)) = pb.run(env).run
        w.take(1).foldLeft(Future.successful(((p, s), o))) { case (acc, evt) => // TODO(jfuentes): take(1) ???
          for {
            res <- acc
            _ <- Future { blocking {
                p.send(new ProducerRecord("test", evt.input)).get
              }}
          } yield res
        }
      }
    }
    val pToQNoop = new (Aux ~> Q) {
      def apply[B](pb: Aux[B]): Q[B] = StateT[Future, Env, B] { case env =>
        val (_, ((p, s), o)) = pb.run(env).run
        Future.successful(((p, s), o))
      }
    }

    val extend = new (P ~> Aux) {
      def apply[A](inner: P[A]): Aux[A] = StateT[W, Env, A] { case (p, s) =>
        inner.run(s) map {
          case (s2, o) => ((p, s2), o)
        }
      }
    }

    def natPAux(natP: ADT ~> P) = new (ADT ~> Aux) {
      def apply[A](adt: ADT[A]): Aux[A] = StateT[W, Env, A] { env =>
        (adt |> (natP(_)) |> extend).run(env)
      }
    }

    def natAuxQ(natAux: ADT ~> Aux) = new (ADT ~> Q) {
      def apply[A](adt: ADT[A]): Q[A] =
        natAux(adt) |>
          (if (CQRS.kind(adt) == core.CQRS.Command)
            pToQ
          else
            pToQNoop)
    }

    to[P](internal) |> (natPAux(_)) |> (natAuxQ(_)) |> (from[Q](_))
  }

  def recover[TC[_[_]], S](
      iso: core.Iso[TC])(
      ser: core.CirceSerializer[iso.ADT],
      instance: TC[State[S, ?]])(consumer: Consumer[Unit, String])(initialState: S) = {
    type P[A] = State[S, A]
    import scala.collection.JavaConverters._
    consumer.assign(List(new TopicPartition("test", 0)).asJava)
    consumer.seek(new TopicPartition("test", 0), 0)
    val evts = consumer.poll(1000).records("test").asScala.map(_.value) // TODO(jfuentes): Read all evts, not only 1000
    // println(s"EVTS(${evts.size}):\n\t${evts.mkString("\n\t")}")
    val evtsDeserialized =
      evts.map(_ |> parse |> (_.fold(throw _, identity)) |> (ser.fromJSON(_)))

    evtsDeserialized.foldLeft(initialState) {
      case (s, adt) => iso.to[P](instance)(adt).exec(s)
    }
    // TODO(jfuentes): Close consumer?
  }

}
