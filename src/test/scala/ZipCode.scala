package org.hablapps.stateless
package test

import scalaz.State

import core.nat._

// Data layer

trait Department[Dp] {
  type P[_]
  type Pr

  val budget: LensAlg[P, Long]
  val person: Person[Pr]
  val people: TraversalAlg.Aux[P, person.P, Pr]
}

trait Person[Pr] {
  type P[_]
  type Ad

  val name: LensAlg[P, String]
  val address: Address[Ad]
  val optAddress: OptionalAlg.Aux[P, address.P, Ad]
}

trait Address[Ad] {
  type P[_]

  val city: LensAlg[P, String]
  val zip: LensAlg[P, Int]
}

// View layer

trait View[P[_]] {
  def modifyZip(f: Int => Int): P[Unit]
}

object View {

  def fromData[Dp](dep: Department[Dp]): View[dep.P] = new View[dep.P] {
    import dep._, person.{ P => _, _ }, address.{ P => _, _ }

    def modifyZip(f: Int => Int): P[Unit] =
      (people composeOptional optAddress composeLens zip).modify(f)
  }

  // In-memory instance (State + Monocle)

  import monocle.macros.Lenses
  import monocle.std.option.some
  import monocle.function.Each._

  import smonocle.nat.all._

  @Lenses case class SDepartment(budget: Long, people: List[SPerson])

  @Lenses case class SPerson(name: String, address: Option[SAddress])

  @Lenses case class SAddress(city: String, zip: Int)

  def fromState: View[State[SDepartment, ?]] = fromData(stateDepartment)

  val stateDepartment = new Department[SDepartment] {
    type P[X] = State[SDepartment, X]
    type Pr = SPerson
    val budget = asLens(SDepartment.budget)
    val person = new Person[Pr] {
      type P[X] = State[Pr, X]
      type Ad = SAddress
      val name = asLens(SPerson.name)
      val address = new Address[Ad] {
        type P[X] = State[Ad, X]
        val city = asLens(SAddress.city)
        val zip = asLens(SAddress.zip)
      }
      val optAddress = asOptional(SPerson.address composePrism some)
    }
    val people = asTraversal(SDepartment.people composeTraversal each)
  }
}
