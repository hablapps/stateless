// package org.hablapps.stateless
// package test
// package university

// import org.scalatest._

// import scalaz.{ MonadError, \/ }

// import monocle.macros.Lenses
// import monocle.function.all._

// import core.nat.lib.MapAlg
// import smonocle.nat.all._

// import puretest, puretest._, puretest.Filter.Syntax._

// import StateTests._

// class StateTests extends FunSpec with Matchers with MyTest[Program, SUniversity] {

//   implicit val MonadErrorP =
//     stateTMonadError[SUniversity, Throwable \/ ?, Throwable]

//   lazy val uni: University.WithP[Program, SUniversity] = SUniversity.model

//   val FilterP: puretest.Filter[Program] = puretest.Filter[Program]

//   val Tester: StateTester[Program, SUniversity, Throwable] =
//     StateTester.StateTStateTester[Throwable, Throwable \/ ?, SUniversity]
// }

// import scalaz._, Scalaz._

// object StateTests {
//   type Program[A] = StateT[Throwable \/ ?, SUniversity, A]
//   type DProgram[A] = StateT[Throwable \/ ?, SDepartment, A]
//   type LProgram[A] = StateT[Throwable \/ ?, SLecturer, A]
// }

// @Lenses case class SUniversity(name: String, departments: Map[String, SDepartment])
// @Lenses case class SDepartment(budget: Int, lecturers: List[SLecturer])
// @Lenses case class SLecturer(firstName: String, lastName: String, salary: Int)

// object SUniversity {
//   import core.nat.LensAlg, core.nat.op.At

//   def model: University.Aux[Program, SUniversity, SDepartment] =
//     University[Program, SUniversity, DProgram, SDepartment](
//       asLensField[Throwable \/ ?, SUniversity, String](SUniversity.name),
//       SDepartment.model,
//       MapAlg[Program, DProgram, String, SDepartment](
//         fromAtStateT[Throwable \/ ?, SUniversity, String, SDepartment](
//           asLensAlg[Throwable \/ ?, SUniversity, Map[String, SDepartment]](
//             SUniversity.departments)),
//         fromFilterIndexStateT[Throwable \/ ?, SUniversity, String, SDepartment](
//           asLensAlg[Throwable \/ ?, SUniversity, Map[String, SDepartment]](
//             SUniversity.departments))
//         )(StateT.stateTMonadState[SUniversity, Throwable \/ ?]),
//       name => new SUniversity(name, Map.empty))
// }

// object SDepartment {

//   def model: Department.Aux[DProgram, SDepartment, SLecturer] =
//     Department[DProgram, SDepartment, LProgram, SLecturer](
//       SLecturer.model,
//       fromTraversal[Throwable \/ ?, SDepartment, SLecturer](
//         SDepartment.lecturers.composeTraversal(each)),
//       asLensField[Throwable \/ ?, SDepartment, Int](SDepartment.budget),
//       (i, xs) => SDepartment(i, xs.map((SLecturer.apply _).tupled)))
// }

// object SLecturer {

//   def model: Lecturer.WithP[LProgram, SLecturer] =
//     Lecturer[LProgram, SLecturer](
//       asLensField[Throwable \/ ?, SLecturer, String](SLecturer.firstName),
//       asLensField[Throwable \/ ?, SLecturer, String](SLecturer.lastName),
//       asLensField[Throwable \/ ?, SLecturer, Int](SLecturer.salary),
//       (f, l, s) => SLecturer(f, l, s))
// }
