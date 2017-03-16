package org.hablapps.phoropter
package core
package test

import org.scalatest._
import scalaz.{ Lens => _, IndexedStateT => IStateT, _ }, Scalaz._

class LensTest extends FlatSpec with Matchers with LensTestEvs {

  case class Person(name: String, last: String, age: Int, address: Address)

  case class Address(street: String, city: String, number: Int)

  val ageLn = new Lens[State[Person, ?], State[Int, ?], Int] {
    val G = Getter.stateInstance[State[Int, ?], Int]
    val M = Modifiable.stateInstance[State[Int, ?], Int]
    val F = Functor[State[Int, ?]]
    def modifyF[O](f: State[Int, O]): State[Person, O] =
      State(p => (p.copy(age = f(p.age)._1), f(p.age)._2))
  }

  val john = Person("John", "Doe", 40, Address("street", "city", 1))

  "Lens" should "get" in {
    ageLn.get.apply(john) shouldBe (john, john.age)
  }

  it should "modify" in {
    ageLn.modify(_ + 1).apply(john) shouldBe (john.copy(age = john.age + 1), ())
  }

  it should "set" in {
    ageLn.set(30).apply(john) shouldBe (john.copy(age = 30), ())
  }

  val addrLn = new Lens[State[Person, ?], State[Address, ?], Address] {
    val G = Getter.stateInstance[State[Address, ?], Address]
    val M = Modifiable.stateInstance[State[Address, ?], Address]
    val F = Functor[State[Address, ?]]
    def modifyF[O](f: State[Address, O]): State[Person, O] =
      State(p => (p.copy(address = f(p.address)._1), f(p.address)._2))
  }

  val nmbLn = new Lens[State[Address, ?], State[Int, ?], Int] {
    val G = Getter.stateInstance[State[Int, ?], Int]
    val M = Modifiable.stateInstance[State[Int, ?], Int]
    val F = Functor[State[Int, ?]]
    def modifyF[O](f: State[Int, O]): State[Address, O] =
      State(a => (a.copy(number = f(a.number)._1), f(a.number)._2))
  }

  it should "compose" in {
    val ln = addrLn.compose[State[Int, ?], Int](nmbLn)

    ln.get.apply(john) shouldBe (john, john.address.number)

    ln.modify(_ + 1).apply(john) shouldBe (john.copy(
      address = john.address.copy(
        number = john.address.number + 1)), ())

    ln.set(3).apply(john) shouldBe (john.copy(
      address = john.address.copy(number = 3)), ())
  }

  val txtTr = new Traversal[State[Address, ?], State[String, ?], String] {
    val G = Getter.stateInstance[State[String, ?], String]
    val M = Modifiable.stateInstance[State[String, ?], String]
    val F = Functor[State[String, ?]]
    def modifyF[O](
        qo: State[String, O]): State[Address, List[O]] = State { a =>
      val (street2, os) = qo(a.street)
      val (city2, oc) = qo(a.city)
      (a.copy(street = street2, city = city2), List(os, oc))
    }
  }

  it should "compose with Traversals" in {
    val tr = addrLn.compose[State[String, ?], String](txtTr)

    tr.getAll.apply(john) shouldBe
      (john, List(john.address.street, john.address.city))

    tr.modify(_.toUpperCase).apply(john) shouldBe
      (john.copy(address = john.address.copy(
        street = john.address.street.toUpperCase,
        city = john.address.city.toUpperCase)), ())

    tr.set("*").apply(john) shouldBe
    (john.copy(address = john.address.copy(street = "*", city = "*")), ())
  }
}

// Profunctor nonsense :)
trait LensTestEvs {

  implicit def istateStrong[F[_]: Monad, O] = new Strong[IStateT[F, ?, ?, O]] {
    def mapfst[A, B, C](st: IStateT[F, A, B, O])(f: C => A): IStateT[F, C, B, O] =
      ???
    def mapsnd[A, B, C](st: IStateT[F, A, B, O])(f: B => C): IStateT[F, A, C, O] =
      ???
    def first[A, B, C](fa: IStateT[F, A, B, O]): IStateT[F, (A, C), (B, C), O] =
      ???
    def second[A, B, C](fa: IStateT[F, A, B, O]): IStateT[F, (C, A), (C, B), O] =
      ???
  }

  // XXX: can't provide an evidence for polymorphic lens, since there's no
  // instance of `MonadState` for `IndexedState`.
  //
  // XXX: it's difficult to generalise this idea to "any profunctor".
  // `IndexedState` is a special case, because it has three type arguments and
  // match nicely with our own Lens.
  // implicit def stateLens[F[_]: Monad, S, A](
  //     getter: S => A)(
  //     setter: S => A => S): Lens[StateT[F, S, ?], StateT[F, A, ?], A] =
  //   new Lens[StateT[F, S, ?], StateT[F, A, ?], A] {
  //     implicit val MS: MonadState[StateT[F, A, ?], A] =
  //       StateT.stateTMonadState[A, F]
  //     def modifyF[O](qo: StateT[F, A, O]): StateT[F, S, O] = {
  //       implicit val ev: Strong[IStateT[F, ?, ?, O]] = ???
  //       val fst = ev.first[A, A, A => S](qo)
  //       ev.dimap(fst)(
  //         (s: S) => (getter(s), setter(s)))(
  //         { case (a, f) => f(a) })
  //     }
  //   }

  // val ageLn2 = stateLens[Id.Id, Person, Int](_.age)(s => a => s.copy(age = a))
}
