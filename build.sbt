name := "stateless"

scalaVersion := "2.12.3"

crossScalaVersions := Seq("2.11.8", "2.12.1")

organization := "org.hablapps"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.hablapps" %% "puretest-scalaz" % "0.3.2",
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.julien-truffaut" %%  "monocle-core"  % "1.4.0",
  "com.github.julien-truffaut" %%  "monocle-macro" % "1.4.0",
  "org.tpolecat" %% "doobie-core"       % "0.4.2",
  "org.tpolecat" %% "doobie-postgres"   % "0.4.2",
  "org.tpolecat" %% "doobie-scalatest"  % "0.4.2",
  "org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds")

publishTo := version{ v =>
  import java.io.File
  val privateKeyFile: File = new File(sys.env("HOME") + "/.ssh/hablaweb.pem")
  Some(Resolver.sftp(
    "HABLA",
    "repo.hablapps.com",
    "/var/www/repo/html/" + (
      if (v.trim.endsWith("SNAPSHOT")) { "snapshots" } else { "releases" }
    )
  ) as("ubuntu", privateKeyFile))
}.value
