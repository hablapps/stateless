# Stateless

## Overview

Optics provide abstractions and patterns to access and update
immutable data structures. They compose, both homogeneously and
heterogeneously, so they become essential to express complex data
transformations in a modular and elegant way.  However, optics are
restricted to work solely with in-memory data structures.

Stateless is a type class based framework that provides the means to
take optic awesomeness to new settings, such as databases or
microservices. To do so, it exploits *optic algebras*, an abstraction
that generalizes monomorphic optics and enables programmers to
describe the data layer and business logic of their applications in
abstract terms, while keeping them completely decoupled from
particular infrastructures.

## Set Up

Stateless is currently available for Scala 2.12, for the JVM platform.

To get started with SBT, simply add the following to your `build.sbt` file.

```scala
resolvers ++= Seq("Habla releases" at "http://repo.hablapps.com/releases")

libraryDependencies += "org.hablapps" %% "stateless" % "0.1"
```

This library depends on [Monocle](https://github.com/julien-truffaut/Monocle),
[Scalaz](https://github.com/scalaz/scalaz) and
[Shapeless](https://github.com/milessabin/shapeless).

(!) *Soon, we'll publish this library in official repositories, so the resolver 
will become unnecessary*

## Getting Started

Suppose that we wanted to modify the optional zip code field
associated to a person, which in turn belongs to a certain
department. We clearly identity three entities here: department,
person and address. We could implement this logic with case classes
and Monocle as follows:

```scala
import monocle.function.Each._
import monocle.macros.Lenses
import monocle.std.option.some

// Data Layer
@Lenses case class SDepartment(
  budget: Long,
  people: List[SPerson])

@Lenses case class SPerson(
  name: String,
  address: Option[SAddress])

@Lenses case class SAddress(
  city: String,
  zip: Int)

// Business Logic
def modifyZip(f: Int => Int): SDepartment => SDepartment = {
  import SDepartment.people, SPerson.address, SAddress.zip
  (people composeLens address composePrism some composeLens zip).modify(f)
}
```

The resulting implementation for `modifyZip`, while expressing a
complex transformation, is modular and readable. However, we're
constrained to access and mutate in-memory data structures. If we
wanted to persist the state of the application in a relational
database, we should discard optics in favor of the specific
transformations provided by Slick, Doobie, or any other database
framework.  Otherwise, we'd need to pull the whole state, modify it by
means of an optic and finally put it back again, which turns out to be
impractical.

Stateless provides the means to describe the data layer of backend
applications in a decoupled way, exploiting the algebra and modularity
from optics, and later instantiate it to in-memory data structures,
relational databases or any other effectful state-based framework.

#### Decoupled Data Layer

This is the only import that we're gonna need for this example:

```scala
import stateless.core.nat._
```

Stateless adopts type classes to implement application entities. This
is how we encode `Address`:

```scala
trait Address[Ad] {
  type P[_]
  val city: LensAlg[P, String]
  val zip: LensAlg[P, Int]
}
```

As you can see, an address is any type `Ad` for which we can provide
two lenses `city` and `zip`. These fields allow us to access and
modify the city and zip codes of an address `Ad`, respectively. The
particular type of transformation program that it will actually be
employed to access and transform addresses is abstracted away by type
constructor `P`. A typical instantiation for `P` is a state-based
transformation `State[Ad,?]`, in case that we want to store our
application state using case classes. Alternatively, if the
application state is stored in a relational database, we will
typically use a reader-like program, where the read only state refers
to the identifier of the address and the configuration of the database
server.

The second entity is `Person`, that contains a name and optionally an
address.  It's represented this way:

```scala
trait Person[Pr] {
  type P[_]
  type Ad; val Address: Address[Ad]

  val name: LensAlg[P, String]
  val optAddress: OptionalAlg.Aux[P, Address.P, Ad]
}
```

As before, this entity takes a type parameter `Pr` , which represents
the entity state, and a type member `P`, that corresponds with the
program that evolves it.  As this entity contains a name and an
address, we declare two optic algebras: `name` and `optAddress`. There
is nothing remarkable about `name`, but `optAddress`, which is a
so-called optional algebra, brings new patterns.

Specifically, its returning type refers to a generic address type
`Ad`, which is declared as a type member. The fact that we intend this
type to be used as an actual address is declared through the
corresponding type class instance `Address`. Also, the `optAddress`
field's type exposes a second type constructor parameter through the
`Aux` pattern. In fact, every optic algebra hides a type constructor
member `Q`, that represents the type of program that evolves the
focus; optic algebras are then equipped with a natural transformation
that turns these *inner* programs into programs that evolve the whole
`P`, or *outer* programs. Since the focus of the optional field is of
type `Ad`, we use `Address.P` as the type of inner programs for the optional lens. This is
the essential mechanism that enables optic algebra composition in stateless.

Finally, this is how we represent departments:

```scala
trait Department[Dp] {
  type P[_]
  type Pr;  val Person: Person[Pr]

  val budget: LensAlg[P, Long]
  val people: TraversalAlg.Aux[P, Person.P, Pr]
}
```

Besides the new traversal algebra for the `people` field, there's
nothing new in this entity definition. So, our data layer is fully
defined!

(!) *Don't be intimidated by the accidental complexity: type members,
nested entity evidences, etc.. By now, you can simply follow this
pattern. We're working hard on hiding those aspects, to make the
implementation of data layers through stateless as close as possible
to the definition of the corresponding case class.*

#### Decoupled Business Logic

Once we have defined our data layer, it's time for us to implement the
business logic. This is how we modify the zip codes for all the
members of the department in a declarative way:

```scala
def modifyZip[D](f: Int => Int)(Dep: Department[D]): Dep.P[Unit] = {
  import Dep.people, Dep.Person.optAddress, Dep.Person.Address.zip
  (people composeOptional optAddress composeLens zip).modify(f)
}
```

First, compare to the `modifyZip` function for case classes that was
shown previously, this new signature is truly generic. It no longer
works for a specific `SDepartment` case class, but for any type `D`
that qualifies as a department. Similarly, the type of the
transformation program returned by this function is not fixed once and
for all but depends on the actual type of department received. This
level of generality allows us to decouple this implementation from any
concrete infrastructure. And, still, what is great here is that this implementation is almost the same as the one we did for the in-memory scenario with Monocle.

#### Recovering the In-memory Setting

Now, if we want to do something useful with our data layer, we need to
instantiate its implementation in the effectful land. In order to show
that our approach generalizes classic optics, we provide an in-memory
instantiation bellow:

```scala
import smonocle.nat.all._

val stateDepartment = new Department[SDepartment] {
  type P[X] = State[SDepartment, X]

  type Pr = SPerson
  val Person = new Person[Pr] {
    type P[X] = State[Pr, X]

    type Ad = SAddress
    val Address = new Address[Ad] {
      type P[X] = State[Ad, X]

      val city = asLensAlg(SAddress.city)
      val zip = asLensAlg(SAddress.zip)
    }

    val name = asLensAlg(SPerson.name)
    val optAddress = asOptionalAlg(SPerson.address composePrism some)
  }

  val budget = asLensAlg(SDepartment.budget)
  val people = asTraversalAlg(SDepartment.people composeTraversal each)
}
```

If we feed this value to `modifyZip`, we get a `State[SDepartment, Unit]` program, which
updates all the zip codes existing in the department when executed over an
instance of `SDepartment`.

```scala
scala> val initial = SDepartment(1000, List(
     |   SPerson("Juan", Some(SAddress("Leganes", 28911))),
     |   SPerson("Maria", Some(SAddress("Mostoles", 28934)))))
initial: org.hablapps.stateless.test.SDepartment = ...

scala> modifyZip(_ + 1)(stateDepartment).exec(initial)
res0: org.hablapps.stateless.test.SDepartment = SDepartment(1000,List(SPerson(Juan,Some(SAddress(Leganes,28912))), SPerson(Maria,Some(SAddress(Mostoles,28935)))))
```

We can see how the zip codes for Juan and Maria are incremented in one unit.

#### Relational Database Instance

TODO

## Additional Features

Stateless provides additional features that we find interesting for
many implementations:

* It contains *indexed optic algebras*, which turn out to be very handy when
dealing with entities whose parts are indexed.
* It provides typeclasses that produce optic algebras, such as `At` and
`FilterIndex`. They correspond with which is known as *optic functions* in
Monocle.
* Stateless includes recurrent combination of optics. For example, by packaging
a collection of lenses and traversals under certain configuration, we are able
to implement the interface of a Scala `Map`, which is very intuitive from an
object-oriented mindset.
* The library offers utilities to facilitate the instantiation of relevant
frameworks in the Scala ecosystem.

## Limitations

Stateless is still very experimental, and therefore comes with some
limitations.

* We've said that optic algebras generalize optics. However, we have to specify
here that we're generalizing the monomorphic version of optics, instead of the
polymorphic version identified by Russell O'Connor which leads to the classic
`STAB`.
* Our traversal algebra is not really a generalization of a traversal. In fact,
it generalizes what we have considered a *weak traversal*, more restricted but
good enough for most of cases.
* We haven't introduced `Adapter`s or `Prism`s yet. These optics are special
since they are able to build the whole from the part without additional
help, meaning that they don't need to access the current state to evolve the
entity. Therefore, we still have to determine if they make sense in this
context.
* We need to establish what we consider "reasonable" when we talk about
performance. The optimization of instances is crucial to make stateless viable.
* We're working on macro annotations to remove boilerplate from applications.

## Contributing
TODO

# License

Puretest is licensed under the Apache License, Version 2.0.
