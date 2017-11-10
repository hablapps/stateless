// package org.hablapps.stateless
// package test

// import org.scalatest._, prop.Checkers

// import scalaz._, Scalaz._

// import org.scalacheck.Arbitrary, Arbitrary.arbitrary

// import scalacheck.StatelessProperties._

// import monocle.Traversal

// import smonocle.nat.all._

// // import org.hablapps.puretest._

// class TraversalAlgTest extends FlatSpec with Matchers with Checkers {

//   case class Person(name: String, last: String, age: Int)

//   implicit val eqPerson = Equal.equal[Person](_ == _)

//   implicit val arbPerson: Arbitrary[Person] =
//     Arbitrary(
//       for {
//         name <- arbitrary[String]
//         last <- arbitrary[String]
//         age <- arbitrary[Int]
//       } yield Person(name, last, age))

//   implicit val txtTr = fromTraversal[Id, Person, String](new Traversal[Person, String] {
//     def modifyF[F[_]: Applicative](f: String => F[String])(s: Person): F[Person] =
//       (f(s.name) |@| f(s.last)) { Person(_, _, s.age) }
//   })

//   val john = Person("John", "Doe", 40)

//   "Traversal" should "check laws" in {
//     nat.traversalAlg.laws[State[Person, ?], String]
//       .properties.map(_._2).foreach(check(_))
//   }

//   it should "getList" in {
//     txtTr.getList.apply(john) shouldBe (john, List("John", "Doe"))
//   }

//   it should "modify" in {
//     txtTr.modify(_.toUpperCase).apply(john) shouldBe
//       (john.copy(name = john.name.toUpperCase, last = john.last.toUpperCase), ())
//   }

//   it should "set" in {
//     txtTr.set("*").apply(john) shouldBe (john.copy(name = "*", last = "*"), ())
//   }

//   it should "length" in {
//     txtTr.length.apply(john) shouldBe (john, 2)
//   }
// }
