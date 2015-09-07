lazy val root = project.in(file(".")).
  aggregate(metisJS, metisJVM).
  settings(
    publish := {},
    publishLocal := {},
    scalacOptions += "-feature"
  )

name := "metis"
lazy val metis = crossProject.in(file(".")).
  settings(
    name := "metis",
    organization := "net.proofpeer",
    version := "0.2-SNAPSHOT",
    scalaVersion := "2.11.6"
  ).
  jvmSettings(
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3"
  ).
  jsSettings(
    libraryDependencies += "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.3"
  )

lazy val metisJVM = metis.jvm
lazy val metisJS  = metis.js
