package org.hablapps.stateless
package test

import scalaz._, Scalaz._
import doobie.imports._
import monocle.syntax._
import core.nat._, doobieImpl._

import shapeless._

object DoobieSystem{
  val DepartmentLens = LensAlg[ConnectionIO, StateT[ConnectionIO,Int,?],Int]{
    λ[StateT[ConnectionIO,Int,?] ~> ConnectionIO]{
      p => (DepartmentTable.get.unique >>= (p.apply _)) map (_._2)
    }
  }(implicitly, StateT.stateTMonadState[Int,ConnectionIO])
}

object DepartmentTable{

  val createTable: Update0 = sql"""
    CREATE TABLE IF NOT EXISTS Department (
      did SERIAL PRIMARY KEY,
      head INTEGER,
      budget INTEGER NOT NULL
  );""".update

  val dropTable: Update0 = sql"""
    DROP TABLE IF EXISTS Department CASCADE;
  """.update

  def insert(budget: Int): Update0 = sql"""
    INSERT INTO Department (budget) VALUES ($budget);
  """.update

  def updateHead(did: Int, head: Int): Update0 = sql"""
    UPDATE Department SET head = $head WHERE did = $did;
  """.update

  def get: Query0[Int] = sql"""
    SELECT did FROM Department;
  """.query[Int]

  val DLensBudget = DoobieLens[Int,Int]("Department","did","budget")

  val DLensHead = DoobieLens[Int,Int]("Department","did","head")
}

object PersonTable{ // } extends DoobieSchemaKV[Int,(String,Int,Int)]{

  val createTable: Update0 = sql"""
    CREATE TABLE IF NOT EXISTS Person(
      pid SERIAL PRIMARY KEY,
      name VARCHAR(10) NOT NULL,
      did INTEGER NOT NULL,
      add INTEGER,
      FOREIGN KEY (did) REFERENCES Department(did)
  );""".update

  val dropTable: Update0 = sql"""
    DROP TABLE IF EXISTS Person CASCADE;
  """.update

  def insertWithAdd(name: String, did: Int, add: Int): Update0 =
    sql"""
      INSERT INTO Person (name, did, add) VALUES ($name, $did, $add);
    """.update

  def insert(name: String, did: Int): Update0 =
    sql"""
      INSERT INTO Person (name, did) VALUES ($name, $did);
    """.update

  val DLensName = DoobieLens[Int,String]("Person","pid","name")
}

object AddressTable{

  val createTable: Update0 = sql"""
    CREATE TABLE IF NOT EXISTS Address(
      aid SERIAL PRIMARY KEY,
      city VARCHAR(10) NOT NULL,
      zip INTEGER NOT NULL
  );""".update

  val dropTable: Update0 = sql"""
    DROP TABLE IF EXISTS Address;
  """.update

  def insert(city: String, zip: Int): Update0 =
    sql"""
      INSERT INTO Address VALUES ($city, $zip);
    """.update

  val DLensCity = DoobieLens[Int,String]("Person","pid","city")
  val DLensZip = DoobieLens[Int,Int]("Person","pid","zip")
}

object ZipCodeDoobie{

  def doobieView: View[ConnectionIO] =
    View.fromData(doobieSystem)

  def doobieSystem = new SystemData[ConnectionIO]{
    type Dp = Int
    val Department = DoobieDepartment

    val create: ConnectionIO[Unit] =
      DepartmentTable.createTable.run *>
      PersonTable.createTable.run *>
      AddressTable.createTable.run.as(())

    def destroy =
      DepartmentTable.dropTable.run *>
      PersonTable.dropTable.run *>
      AddressTable.dropTable.run.as(())

    def initAddress(ad: SAddress): ConnectionIO[Int] =
      AddressTable.insert(ad.city,ad.zip).withUniqueGeneratedKeys[Int]("aid")

    def initPerson(did: Int)(p: SPerson): ConnectionIO[Int] =
      PersonTable.insert(p.name,did).withUniqueGeneratedKeys[Int]("pid")
      // p.address.traverse(initAddress) >>= {
      //   _.fold(PersonTable.insert(p.name,did).withUniqueGeneratedKeys[Int]("pid")){
      //     aid => PersonTable.insertWithAdd(p.name,did,aid).withUniqueGeneratedKeys[Int]("pid")
      //   }
      // }

    def init(dep: SDepartment): ConnectionIO[Unit] = for {
      did <- DepartmentTable.insert(dep.budget).withUniqueGeneratedKeys[Int]("did")
      pid <- initPerson(did)(dep.head)
      _ <- DepartmentTable.updateHead(did,pid).run
      _ <- dep.members.traverseU(initPerson(did))
    } yield ()

    val department = DoobieSystem.DepartmentLens
  }

  type DoobieConf = doobie.util.transactor.Transactor.fromDriverManager.type

  object DoobieDepartment extends Department[Int]{
    type P[T]=StateT[ConnectionIO,Int,T]
    type Pr=Int
    val Person = DoobiePerson

    val budget: LensAlg[P, Int] = LensState.fromDLens(DepartmentTable.DLensBudget)
    val head: LensAlg.Aux[P, Person.P, Pr] = LensState.fromStateT(DepartmentTable.DLensHead)
    lazy val members: TraversalAlg.Aux[P, Person.P, Pr] = ???
  }

  object DoobiePerson extends Person[Int]{
    type P[T]=StateT[ConnectionIO,Int,T]
    type Ad=Int
    val Address: Address[Ad] = DoobieAddress

    val name: LensAlg[P, String] = LensState.fromDLens(PersonTable.DLensName)
    lazy val optAddress: OptionalAlg.Aux[P, Address.P, Ad] = ???
  }

  object DoobieAddress extends Address[Int]{
    type P[T]=StateT[ConnectionIO,Int,T]

    val city: LensAlg[P, String] = LensState.fromDLens(AddressTable.DLensCity)
    val zip: LensAlg[P, Int] = LensState.fromDLens(AddressTable.DLensZip)
  }

}