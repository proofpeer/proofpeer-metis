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
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "latest.integration",
    initialCommands in console := """
import proofpeer.metis.fol.sttzf.Axioms._
import org.ensime.sexp.{ SexpCompactPrinter => SexpPrinter }
import proofpeer.metis.fol.SExpr.{ SExprOfFol }
import proofpeer.metis.fol._
import org.ensime.sexp._
import scalaz._
import Scalaz._
import proofpeer.metis.Clause
import proofpeer.metis.ClauseInstances._
"""
  )
lazy val metisJVM = metis.jvm
lazy val metisJS  = metis.js
