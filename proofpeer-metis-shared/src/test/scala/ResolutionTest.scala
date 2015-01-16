package proofpeer.metis.testing

import proofpeer.metis._
import org.scalatest.{FlatSpec}
import org.scalactic.Tolerance._
import scalaz._
import Scalaz._

class ResolutionSpec extends FlatSpec {
  implicit val ordFun = KnuthBendix.precendenceOrder[String,String]
  val kbo    = new KnuthBendix[String,String]((_,_) => 1)
  val litOrd = new LiteralOrdering[String,String,String](kbo,x => x)
  val kernel = new Kernel[String,String,String]
  val factor = new Factor[String,String,String]

  def tryProblem(
    problemName: String,
    litss: List[List[Literal[String,String,String]]],
    expectedSteps: Int,
    tolerance: Double) = {

    // Build a new itheorem factory so that id counting starts again at 0.
    val ithmF  = new IThmFactory[String,String,String,Int,kernel.type](
      kernel,
      0,
      n => (n+1, "genvar" + n),
      litOrd,
      factor)

    val clauses = litss.map { lits => Clause(lits.toSet) }
    val sys = new Resolution(0L,clauses,kbo,ithmF)
    val pulls      = sys.dpulled.takeWhile { thm2 => !(thm2._2.get.isContradiction) }
    val pullsLimit = pulls.take(((tolerance + 1) * expectedSteps + 1).toInt)
    val noSteps    = pullsLimit.length
    val discrepancy    = Math.abs(noSteps - expectedSteps)
    val maxDiscrepency = expectedSteps * tolerance
    if (maxDiscrepency > 0) {
      problemName should ("be proven in " + expectedSteps + " steps") in {
        assert(noSteps.toDouble === expectedSteps.toDouble +- maxDiscrepency)
      }
    }
    else {
      problemName should ("be proven in " + expectedSteps + " steps") in {
        assert(noSteps == expectedSteps)
      }
    }
  }

  // Without model-checking

  tryProblem(
    "Set Theory Problem 1",
    tptp.Problems.SET.SET001_MINUS1.clauses,
    16,
    0)

  tryProblem(
    "Set Theory Problem 2",
    tptp.Problems.SET.SET002_MINUS1.clauses,
    48,
    0)

  tryProblem(
    "Set Theory Problem 3",
    tptp.Problems.SET.SET003_MINUS1.clauses,
    25,
    0)

  tryProblem(
    "Set Theory Problem 4",
    tptp.Problems.SET.SET004_MINUS1.clauses,
    26,
    0)

  tryProblem(
    "Set Theory Problem 5",
    tptp.Problems.SET.SET005_MINUS1.clauses,
    508,
    0)

  tryProblem(
    "Set Theory Problem 6",
    tptp.Problems.SET.SET006_MINUS1.clauses,
    25,
    0)

  tryProblem(
    "Set Theory Problem 7",
    tptp.Problems.SET.SET007_MINUS1.clauses,
    1117,
    0)

  tryProblem(
    "Set Theory Problem 8",
    tptp.Problems.SET.SET008_MINUS1.clauses,
    91,
    0)

  tryProblem(
    "Set Theory Problem 9",
    tptp.Problems.SET.SET009_MINUS1.clauses,
    162,
    0)

  tryProblem(
    "Set Theory Problem 10",
    tptp.Problems.SET.SET010_MINUS1.clauses,
    2239,
    0)

  // With model checking

  // tryProblem(
  //   "Set Theory Problem 1",
  //   tptp.Problems.SET.SET001_MINUS1.clauses,
  //   17,
  //   0)

  // tryProblem(
  //   "Set Theory Problem 2",
  //   tptp.Problems.SET.SET002_MINUS1.clauses,
  //   48,
  //   0)

  // tryProblem(
  //   "Set Theory Problem 3",
  //   tptp.Problems.SET.SET003_MINUS1.clauses,
  //   25,
  //   0)

  // tryProblem(
  //   "Set Theory Problem 4",
  //   tptp.Problems.SET.SET004_MINUS1.clauses,
  //   26,
  //   0)

  // tryProblem(
  //   "Set Theory Problem 5",
  //   tptp.Problems.SET.SET005_MINUS1.clauses,
  //   477,
  //   0)

  // tryProblem(
  //   "Set Theory Problem 6",
  //   tptp.Problems.SET.SET006_MINUS1.clauses,
  //   25,
  //   0)

  // tryProblem(
  //   "Set Theory Problem 7",
  //   tptp.Problems.SET.SET007_MINUS1.clauses,
  //   871,
  //   0)

  // tryProblem(
  //   "Set Theory Problem 8",
  //   tptp.Problems.SET.SET008_MINUS1.clauses,
  //   90,
  //   0)

  // tryProblem(
  //   "Set Theory Problem 9",
  //   tptp.Problems.SET.SET009_MINUS1.clauses,
  //   186,
  //   0)

  // tryProblem(
  //   "Set Theory Problem 10",
  //   tptp.Problems.SET.SET010_MINUS1.clauses,
  //   2338,
  //   0)
}
