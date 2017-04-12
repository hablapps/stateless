name := "stateless"

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.12.1")

organization := "org.hablapps"

version := "0.1"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "com.github.julien-truffaut" %%  "monocle-core"  % "1.4.0",
  "com.github.julien-truffaut" %%  "monocle-macro" % "1.4.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds")
