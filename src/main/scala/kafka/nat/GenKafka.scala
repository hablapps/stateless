package org.hablapps.stateless
package kafka
package nat

import org.apache.kafka.clients.producer.{Producer, ProducerRecord}
import scala.concurrent.{blocking, Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scalaz._
import scalaz.std.list._
import scalaz.std.scalaFuture._
import scalaz.syntax.id._

object GenKafka {

  type JSON = writer.nat.GenWriter.ButtonPressed

  def stateT[TC[_[_]], S](
      iso: core.Iso[TC],
      internal: TC[StateT[Writer[List[JSON], ?], S, ?]])(implicit
      EC: ExecutionContext): TC[StateT[Future, (Producer[Unit, String], S), ?]] = {

    import iso._

    type Env    = (Producer[Unit, String], S)
    type W  [X] = Writer[List[JSON], X]
    type P  [X] = StateT[     W,   S, X]
    type Aux[X] = StateT[     W, Env, X]
    type Q  [X] = StateT[Future, Env, X]

    val qToAux = new (Q ~> Aux) {
      def apply[B](qb: Q[B]): Aux[B] = StateT[W, Env, B] { case (p, s) =>
        val ((p2, s2), o) = Await.result(qb.run((p, s)), 3 seconds)
        Writer(List.empty, ((p2, s2), o))
      }
    }
    val pToQ = new (Aux ~> Q) {
      def apply[B](pb: Aux[B]): Q[B] = StateT[Future, Env, B] { case env =>
        val (w, ((p, s), o)) = pb.run(env).run
        w.foldLeft(Future.successful(((p, s), o))) { case (acc, evt) =>
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

    def natPAux(natP: ADT[P, ?] ~> P) = new (ADT[Aux, ?] ~> Aux) {
      def apply[A](adtP: ADT[Aux, A]): Aux[A] = StateT[W, Env, A] { env =>
        val foo = new (Aux ~> P) {
          def apply[B](sAux: Aux[B]): P[B] = StateT[W, S, B] { s =>
            sAux.run(env).map { case ((_, s2), b) => (s2, b) }
          }
        }
        (mapHK(foo)(adtP) |> (natP(_)) |> extend).run(env)
      }
    }

    def natAuxQ(natAux: ADT[Aux, ?] ~> Aux) = new (ADT[Q, ?] ~> Q) {
      def apply[A](adtQ: ADT[Q, A]): Q[A] = {
        val adtAux = mapHK(qToAux)(adtQ)
        natAux(adtAux) |>
          (if (kind(adtAux) == core.Iso.Command)
            pToQ
          else
            pToQNoop)
      }
    }

    to[P](internal) |> (natPAux(_)) |> (natAuxQ(_)) |> from[Q]
  }

  import org.apache.kafka.clients.consumer.Consumer
  import org.apache.kafka.common.TopicPartition
  import io.circe.parser._
  def recover[TC[_[_]], S](
      iso: core.Iso[TC],
      instance: TC[State[S, ?]])(consumer: Consumer[Unit, String])(initialState: S) = {
    type P[A] = State[S, A]
    import scala.collection.JavaConverters._
    consumer.assign(List(new TopicPartition("test", 0)).asJava)
    consumer.seek(new TopicPartition("test", 0), 0)
    val evts = consumer.poll(1000).records("test").asScala.map(_.value) // TODO(jfuentes): Read all evts, not only 1000
    // println(s"EVTS(${evts.size}):\n\t${evts.mkString("\n\t")}")
    val evtsDeserialized =
      evts.map(_ |> parse |> (_.fold(throw _, identity)) |> (iso.fromJSON[P](_)))

    evtsDeserialized.foldLeft(initialState) {
      case (s, adt) => iso.to[P](instance)(adt).exec(s)
    }
    // TODO(jfuentes): Close consumer?
  }

}
