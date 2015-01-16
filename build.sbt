lazy val root = project.in(file(".")).aggregate(rootJS, rootJVM)

lazy val sharedSettings = Seq(
  name := "METIS",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.2",
  scalacOptions += "-feature",
  unmanagedSourceDirectories in Compile +=
    (baseDirectory.value / "..") / "proofpeer-metis-shared" / "src" / "main" / "scala"
)

lazy val rootJS = project.in(file("proofpeer-metis-js"))
  .settings(sharedSettings: _*)
  .settings(scalaJSSettings: _*)
  .settings(
  libraryDependencies +=  "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "latest.integration"
)

lazy val rootJVM = project.in(file("proofpeer-metis-jvm"))
  .settings(
    name := "METIS",
    organization := "net.proofpeer",
    scalaVersion := "2.11.2",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "latest.integration",
    libraryDependencies += "org.scalatest" %% "scalatest" % "latest.integration",
    unmanagedSourceDirectories in Compile +=
      (baseDirectory.value / "..") / "proofpeer-metis-shared" / "src" / "main" / "scala"
)
