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
    addCompilerPlugin("org.spire-math" % "kind-projector" %
      "0.9.3" cross CrossVersion.binary),
    libraryDependencies += "org.ensime" %% "s-express" % "2.0.0-SNAPSHOT",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "latest.integration"
  )
lazy val metisJVM = metis.jvm
lazy val metisJS  = metis.js
