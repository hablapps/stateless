// package org.hablapps.stateless
// package test
//
// import org.scalatest._
//
// import scalaz._, Scalaz._
//
// import monocle.Traversal
//
// import smonocle.nat.all._
//
// class TraversalAlgTest extends FlatSpec with Matchers {
//
//   case class Person(name: String, last: String, age: Int)
//
//   val txtTr = fromTraversal[Id, Person, String](new Traversal[Person, String] {
//     def modifyF[F[_]: Applicative](f: String => F[String])(s: Person): F[Person] =
//       (f(s.name) |@| f(s.last)) { Person(_, _, s.age) }
//   })
//
//   val john = Person("John", "Doe", 40)
//
//   "Traversal" should "getList" in {
//     txtTr.getList.apply(john) shouldBe (john, List("John", "Doe"))
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
//   it should "length" in {
//     txtTr.length.apply(john) shouldBe (john, 2)
//   }
// }
