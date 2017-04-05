// package org.hablapps.phoropter
// package core
// package test
//
// import org.scalatest._
// import scalaz.{ Lens => _, _ }, Scalaz._
//
// class TraversalTest extends FlatSpec with Matchers {
//
//   case class Person(name: String, last: String, age: Int)
//
//   val txtTr = new Traversal[State[Person, ?], State[String, ?], String] {
//     val G = Getter.stateInstance[State[String, ?], String]
//     val M = Modifiable.stateInstance[State[String, ?], String]
//     val F = Functor[State[String, ?]]
//     def modifyF[O](
//         qo: State[String, O]): State[Person, List[O]] = State { p =>
//       val (name2, on) = qo(p.name)
//       val (last2, ol) = qo(p.last)
//       (p.copy(name = name2, last = last2), List(on, ol))
//     }
//   }
//
//   val john = Person("John", "Doe", 40)
//
//   "Traversal" should "getAll" in {
//     txtTr.getAll.apply(john) shouldBe (john, List("John", "Doe"))
//   }
//
//   it should "modify" in {
//     txtTr.modify(_.toUpperCase).apply(john) shouldBe
//       (john.copy(name = john.name.toUpperCase, last = john.last.toUpperCase), ())
//   }
//
//   it should "set" in {
//     txtTr.set("*").apply(john) shouldBe (john.copy(name = "*", last = "*"), ())
//   }
//
//   it should "count" in {
//     txtTr.count.apply(john) shouldBe (john, 2)
//   }
// }
