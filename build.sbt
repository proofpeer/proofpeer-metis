name := "metis"
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
    scalaVersion in ThisBuild := "2.11.8",
    libraryDependencies += "org.ensime" %% "s-express" % "2.0.0-SNAPSHOT"
  ).
  jvmSettings(
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "latest.integration"
  ).
  jsSettings(
    libraryDependencies += "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "latest.integration"
  )
lazy val metisJVM = metis.jvm
lazy val metisJS  = metis.js
