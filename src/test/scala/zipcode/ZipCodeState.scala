package org.hablapps.stateless
package test

import scalaz._, Scalaz._
import monocle.macros.Lenses

@Lenses case class System(department: SDepartment)

@Lenses case class SDepartment(
  budget: Int,
  head: SPerson,
  members: List[SPerson])

@Lenses case class SPerson(
  name: String,
  address: Option[SAddress],
  email: Map[Int, String] = Map.empty[Int, String])

@Lenses case class SAddress(city: String, zip: Int)

object ZipCodeState{

  import core.nat._
  import lib._
  import smonocle.nat.all._
  import monocle.std.option.some
  import monocle.function.Each._

  def stateView[F[_]: Monad]: View[StateT[F, System, ?]] =
    View.fromData[StateT[F,System,?]](stateSystem[F])

  def stateSystem[F[_]: Monad] = new SystemData[StateT[F,System,?]]{
    type Dp = SDepartment
    val Department = stateDepartment[F]

    val create = ().point[StateT[F,System,?]]
    val destroy = ().point[StateT[F,System,?]]
    def init(dep: SDepartment) = MonadState[StateT[F,System,?],System].put(System(dep))

    val department = asLensAlg(System.department)
  }

  //- BEGIN AUX

  implicit class LensOp[S,A](lens: monocle.PLens[S,S,A,A]){
    def :+(trav: monocle.PTraversal[S,S,A,A]) =
      new monocle.PTraversal[S, S, A, A]{
        def modifyF[F[_]: Applicative](f: A => F[A])(s: S): F[S] =
          (lens.modifyF(f)(s) |@| trav.modifyF(f)(s)){
            (s1,s2) => lens.set(lens.get(s1))(s2)
          }
    }
  }

  //- END AUX

  def stateDepartment[F[_]: Monad] = new Department[SDepartment] {
    type P[X] = StateT[F,SDepartment, X]
    type Pr = SPerson
    val Person = statePerson[F]

    val budget = asLensAlg(SDepartment.budget)
    val head = asLensAlg(SDepartment.head)
    val members = fromTraversal(SDepartment.head :+ (SDepartment.members composeTraversal each))
  }

  def statePerson[F[_]: Monad] = new Person[SPerson] {
    type P[X] = StateT[F,SPerson, X]
    type Ad = SAddress
    val Address = stateAddress[F]

    val name = asLensAlg(SPerson.name)
    val optAddress = fromOptional(SPerson.address composePrism some)
    val emailMap = mapFromMapMLens[F, SPerson, Int, String](SPerson.email)
  }

  def stateAddress[F[_]: Monad] = new Address[SAddress] {
    type P[X] = StateT[F,SAddress, X]

    val city = asLensAlg(SAddress.city)
    val zip = asLensAlg(SAddress.zip)
  }

}
