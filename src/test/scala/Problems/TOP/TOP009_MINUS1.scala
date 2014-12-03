package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP009_MINUS1{
  val mainClauses =
    List(
      // Name: problem_4_120
      // Role: negated_conjecture
      List("open(cy, cx, ct)"),
      // Name: problem_4_121
      // Role: negated_conjecture
      List("open(a, cy, subspace_topology(cx, ct, cy))"),
      // Name: problem_4_122
      // Role: negated_conjecture
      List("~open(a, cx, ct)"))
  
  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
