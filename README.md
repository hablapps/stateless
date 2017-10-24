# Stateless

## Overview

Optics provide abstractions and patterns to access and update immutable data
structures. They compose, both homogeneously and heterogeneously, so they become
essential to express complex data transformations in a modular and elegant way.
However, optics are restricted to work solely with in-memory data structures.

Stateless provides the means to take optic awesomeness to new settings, such as
databases or microservices. To do so, it adopts *optic algebras*, an abstraction
that generalizes monomorphic optics. They enable programmers to describe the
data layer and business logic of their applications while keeping them
completely decoupled from particular infrastructures.

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

## Getting Started

Suppose that we wanted to modify the optional zip code field associated to a
person, which in turn belongs to certain department. We clearly identity three
entities here: department, person and address. We could implement this logic in
Monocle as follows:

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

The resulting implementation for `modifyZip` is modular and readable, though
expressing a complex transformation. However, we're constrained to access and
mutate in-memory data structures. If we wanted to persist the state of the
application in a relational database, we should discard optics in favor of the
specific transformations provided by the particular technology. Otherwise, we'd
need to pull the whole state, modify it by means of an optic and finally put it
back again, which turns out to be impractical.

Stateless provides the means to describe the data layer and the business logic
in a decoupled way, exploiting the algebra and modularity from optics, and later
instantiate it to in-memory data structures, relational databases or any other
effectful state-based framework.

#### Decoupled Data Layer

We will be using the *natural* representation of optic algebras, which heavily
relies on natural transformations. Therefore, this is the only import that we're
gonna need for this example:

```scala
import stateless.core.nat._
```

Stateless adopts the object-oriented style to define the application entities.
Thereby, each entity will be defined in its corresponding trait. This is how we
encode `Address`:

```scala
trait Address[Ad] {
  type P[_]
  val city: LensAlg[P, String]
  val zip: LensAlg[P, Int]
}
```

As you can see, there is a lens algebra for every field in the entity. Lens
algebras take a type constructor as first parameter. It corresponds with the
kind of programs that we are interested in for this entity. Particularly, `P`
determines the type of program that is able to evolve an address. The second
parameter is the focus of the lens, `String` for `city` and `Int` for `zip`.
Finally, `Ad` corresponds with the state which is evolved and hidden by `P`.

The second entity is person, that contains a name and optionally an address.
It's represented this way:

```scala
trait Person[Pr] {
  type P[_]
  type Ad
  val name: LensAlg[P, String]
  val address: Address[Ad]
  val optAddress: OptionalAlg.Aux[P, address.P, Ad]
}
```

As before, this entity takes a type parameter `Pr` , which represents the entity
state, and a type member `P`, that corresponds with the program that evolves it.
As this entity contains a name and an address, we declare two optic algebras:
`name` and `optAddress`. There is nothing remarkable about `name`, but
`optAddress`, which is an optional algebra, brings new patterns.

Specifically, its returning type uses an alias `Aux` that shows a second type
constructor parameter. In fact, every optic algebra hides a type constructor
member `Q`, that roles the program that evolves the focus. Having said so,
natural optic algebras turn programs that evolve the focus `Q`, or *inner*
programs, into programs that evolve the whole `P`, or *outer* programs. Thereby,
we need the reification of `Q` to connect `Person`'s inner program with
`Address`' outer program, to enable optic algebra composition. As a consequence,
we need to provide an evidence of entity `address`, and therefore the type of
its final state `Ad`.

Finally, this is how we represent departments:

```scala
trait Department[Dp] {
  type P[_]
  type Pr
  val budget: LensAlg[P, Long]
  val person: Person[Pr]
  val people: TraversalAlg.Aux[P, person.P, Pr]
}
```

Despite the appearance of a traversal algebra, there's nothing new in this
entity definition. So, our data layer is fully defined!

(!) *Don't be intimidated by the accidental complexity: type members, nested
entity evidences, etc.. By now, you can simply follow the pattern. We're working
hard on hiding those aspects, to make our entity definition as close as possible
to the definition of the corresponding case class.*

#### Decoupled Business Logic

Once we have defined our data layer, it's time for us to implement the business
logic. This is how we modify the zip codes for all the members of the department
in a declarative way:

```scala
def modifyZip[Dp](f: Int => Int)(dep: Department[Dp]): dep.P[Unit] = {
  import dep.people, dep.person.optAddress, dep.person.address.zip
  (people composeOptional optAddress composeLens zip).modify(f)
}
```

As you can see, we had to adapt the signature of the function. It no longer
takes an `SDepartment`, it takes a particular instantiation of the department
instead. The function returns the program that makes the department evolv,
modifying the zip codes. What is great here is that the implementation of this
method is exactly the same as the one we used in the in-memory scenario with
Monocle. However, this version is completely decoupled from particular
infrastructures, aspect which is encapsulated in `dep`.

#### Recovering In-memory Setting

The resulting data layer is completely decoupled from any infrastructure.
However, if we want to do something useful with it, we need to land our program
in the effectful land. In order to show that our approach generalizes classic
optics, we provide an in-memory instantiation:

```scala
import smonocle.nat.all._

val stateDepartment = new Department[SDepartment] {
  type P[X] = State[SDepartment, X]
  type Pr = SPerson
  val budget = asLensAlg(SDepartment.budget)
  val person = new Person[Pr] {
    type P[X] = State[Pr, X]
    type Ad = SAddress
    val name = asLensAlg(SPerson.name)
    val address = new Address[Ad] {
      type P[X] = State[Ad, X]
      val city = asLensAlg(SAddress.city)
      val zip = asLensAlg(SAddress.zip)
    }
    val optAddress = asOptionalAlg(SPerson.address composePrism some)
  }
  val people = asTraversalAlg(SDepartment.people composeTraversal each)
}
```

If we feed this value to `modifyZip`, we get a `State[SDepartment, Unit]`, which
updates all the zip codes existing in the department when executed over an
instance of `case class SDepartment`.

```scala
scala> val initial = SDepartment(1000, List(
     |   SPerson("Juan", Some(SAddress("Leganes", 28911))),
     |   SPerson("Maria", Some(SAddress("Mostoles", 28934)))))
initial: org.hablapps.stateless.test.SDepartment = SDepartment(1000,List(SPerson(Juan,Some(SAddress(Leganes,28911))), SPerson(Maria,Some(SAddress(Mostoles,28934)))))

scala> modifyZip(_ + 1)(stateDepartment).exec(initial)
res0: org.hablapps.stateless.test.SDepartment = SDepartment(1000,List(SPerson(Juan,Some(SAddress(Leganes,28912))), SPerson(Maria,Some(SAddress(Mostoles,28935)))))
```

We can see how the zip codes for Juan and Maria are incremented in one unit.

#### Relational Database Instance

TODO

## Additional Features

Stateless provides additional features that we find interesting for many
implementations:

* It contains *indexed optic algebras*, which turn out to be very handy when
dealing with entities whose parts are indexed.
* It provides typeclasses that produce optic algebras, such as `At` and
`FilterIndex`. They correspond with which is known as *optic functions* in
Monocle.
* Stateless includes recurrent combination of optics. For example, by packaging
a collection of lenses and traversals under certain configuration, we are able
to implement the interface of a Scala `Map`, which is very intuitive for an
object-oriented mindset.
* The library offers utilities to facilitate the instantiation of relevant
frameworks in the Scala ecosystem.

## Limitations

Stateless is still very experimental, and therefore comes with some limitations.

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

## License
TODO
