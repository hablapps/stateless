package org.hablapps.stateless
package test
package test2

import scalaz._, Scalaz._

import core.nat._

object Standard{

  // Data layer

  trait Department[D] {
    type P[_]
    type E; val Employee: Employee[E]

    val head: LensAlg.Aux[P, Employee.P, E]
  }

  trait Employee[E] {
    type P[_]

    val salary: LensAlg[P,Int]
  }

  // View layer

  def raiseSalary[D](f: Int)(Department: Department[D]): Department.P[Unit] =
    (Department.head composeLens Department.Employee.salary).modify(_ + f)

  // In-memory instance (State + Monocle)

  import monocle.macros.Lenses
  import monocle.std.option.some
  import monocle.function.Each._

  import smonocle.nat.all._

  @Lenses case class SDepartment(head: SEmployee)
  @Lenses case class SEmployee(salary: Int)

  val stateDepartment = new Department[SDepartment] {
    type P[X] = State[SDepartment, X]

    type E = SEmployee

    val Employee = new Employee[E] {
      type P[X] = State[E, X]

      val salary = asLens(SEmployee.salary)
    }

    val head = asLens(SDepartment.head)
  }

  raiseSalary(5)(stateDepartment): State[SDepartment,Unit]
}

object NoPNoOptics{

  trait Department[D] {
    type E; val Employee: Employee[E]

    def getHead: D => E
    def setHead(e: E): D => D
    def modifyHead(f: E => E): D => D =
      d => setHead(f(getHead(d)))(d)
  }

  trait Employee[E] {
    def getSalary: E => Int
    def setSalary(i: Int): E => E
    def modifySalary(f: Int => Int): E => E =
      e => setSalary(f(getSalary(e)))(e)
  }

  def raiseHeadSalary[D](i: Int)(Department: Department[D]): D => D =
    Department.modifyHead(Department.Employee.modifySalary(_ + i))

}

object NoPOpticsGetSet{

  case class Lens[S,A](
    get: S => A,
    set: A => S => S
  ){
    def modify(f: A => A): S => S =
      s => set(f(get(s)))(s)

    def compose[B](l: Lens[A,B]): Lens[S,B] =
      Lens(get andThen l.get,
        b => s => set(l.set(b)(get(s)))(s))
  }

  trait Department[D] {
    type E; val Employee: Employee[E]

    val head: Lens[D,E]
  }

  trait Employee[E] {
    val salary: Lens[E,Int]
  }

  def raiseHeadSalary[D](i: Int)(Department: Department[D]): D => D =
    (Department.head compose Department.Employee.salary).modify(_ + i)

}

object NoPOpticsModify{

  case class Lens[S,A](
    get: S => A,
    modify: (A => A) => S => S
  ){
    def set(v: A): S => S =
      modify(_ => v)

    def compose[B](l: Lens[A,B]): Lens[S,B] =
      Lens(get andThen l.get,
        l.modify andThen modify)
  }

  case class Traversal[S,A](
    get: S => List[A],
    modify: (A => A) => S => (S, List[Unit])
  ){
    def set(v: A): S => (S, List[Unit]) = 
      modify(_ => v)

    def compose[B](l: Lens[A,B]): Traversal[S,B] = 
      Traversal(get andThen (_.map(l.get)),
        l.modify andThen modify)

    def filter(p: S => A): Traversal[S,A] = 
      Traversal(s => get(s).filter(_ != p(s)),
        m => s => modify(a => if (a!=p(s)) m(a) else a)(s))
  }

  trait Department[D] {
    type E; val Employee: Employee[E]

    val head: Lens[D,E]
    val members: Traversal[D,E]
  }

  trait Employee[E] {
    val salary: Lens[E,Int]
  }

  def raiseHeadSalary[D](i: Int)(Department: Department[D]): D => D =
    // (Department.Employee.salary.modify andThen Department.head.modify)(_ + i)
    (Department.head compose Department.Employee.salary).modify(_ + i)

  def raiseSalaries[D](i: Int)(Department: Department[D]): D => (D, List[Unit]) =
    (Department.members compose Department.Employee.salary).modify(_ + i)

  def raiseNonHeadSalaries[D](i: Int)(Department: Department[D]): D => (D, List[Unit]) =
    (Department.members.filter(Department.head.get) compose Department.Employee.salary).modify(_ + i)

}


object POpticsModify{

  trait Lens[P[_],A]{
    def get: P[A]
    def modify(f: A => A): P[Unit]

    def set(v: A): P[Unit] =
      modify(_ => v)

    def compose[Q[_],B](l: Lens[Q,B])(implicit C: Q ~> P): Lens[P,B] =
      new Lens[P,B]{
        def get: P[B] = C(l.get)
        def modify(f: B => B) = C(l.modify(f))
      }
  }

  trait Department[D] {
    type P[_]
    type E; val Employee: Employee[E]

    val head: Lens[P,E]
  }

  trait Employee[E] {
    type P[_]
    val salary: Lens[P,Int]
  }

  def raiseHeadSalary[D](i: Int)(Department: Department[D])(
    implicit C: Department.Employee.P ~> Department.P): Department.P[Unit] =
    (Department.head.compose(Department.Employee.salary)(C)).modify(_ + i)

}


object POpticsGetSet{

  trait Lens[P[_],A]{
    def get: P[A]
    def set(a: A): P[Unit]

    def modify(f: A => A)(implicit M: Monad[P]): P[Unit] =
      get >>= { a => set(f(a)) }

    def modifyF[Q[_],T](f: Q[T])(implicit C: Q ~> P): P[T] =
      C(f)

    def compose[Q[_],B](l: Lens[Q,B])(implicit C: Q ~> P): Lens[P,B] =
      new Lens[P,B]{
        def get: P[B] = C(l.get: Q[B])
        def set(b: B): P[Unit] = C(l.set(b))
      }
  }

  trait Department[D] {
    type P[_]
    type E; val Employee: Employee[E]

    val head: Lens[P,E]
  }

  trait Employee[E] {
    type P[_]
    val salary: Lens[P,Int]
  }

  def raiseHeadSalary[D](i: Int)(
      Department: Department[D])(implicit
      C: Department.Employee.P ~> Department.P,
      M: Monad[Department.P]): Department.P[Unit] =
    (Department.head.compose(Department.Employee.salary)(C)).modify(_ + i)(M)

}

object POptics{

  case class Lens[P[_],Q[_]](modify: Q ~> P){

    // val MS: MonadState[Q,A]
    // def get: P[A] = modify(MS.get)
    // def set(v: A): P[Unit] = modify(MS.put(v))
    // def modify(f: A => A): P[Unit] = modify(MS.modify(f))

    def compose[R[_]](l: Lens[Q,R]): Lens[P,R] =
      Lens[P,R](l.modify andThen modify)
  }

  case class Traversal[P[_],Q[_]](
    modify: Q ~> λ[t => P[List[t]]]){

    def compose[R[_]](l: Lens[Q,R]): Traversal[P,R] = 
      Traversal[P,R](l.modify andThen[λ[t=>P[List[t]]]] modify)

    def filter(l: Lens[P,Q]): Traversal[P,Q] = 
      ???
  }

  trait Department[D[_]] {
    type E[_]; val Employee: Employee[E]

    val head: Lens[D,E]
    val members: Traversal[D,E]
  }

  type IntState[t] = State[Int,t]

  trait Employee[E[_]] {
    val salary: Lens[E,IntState]
  }

  def raiseHeadSalary[D[_]](i: Int)(Department: Department[D]): D[List[Unit]] =
    (Department.members compose Department.Employee.salary)
      .modify(State.modify(_ + i))

}
