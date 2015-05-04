lazy val root = project.in(file(".")).
aggregate(metisJS, metisJVM).
settings(
publish := {},
publishLocal := {},
scalacOptions += "-feature"
)
lazy val metis = crossProject.in(file(".")).
settings(
name := "metis",
organization := "net.proofpeer",
version := "0.2-SNAPSHOT",
scalaVersion := "2.11.6"
).
jvmSettings(
libraryDependencies += "org.scalaz" %% "scalaz-core" % "latest.integration",
libraryDependencies += "org.scalatest" %% "scalatest" % "latest.integration"
).
jsSettings(
libraryDependencies += "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.1" )
lazy val metisJVM = metis.jvm
lazy val metisJS = metis.js
