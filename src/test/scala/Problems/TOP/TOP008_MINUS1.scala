package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP008_MINUS1{
  val mainClauses =
    List(
      // Name: problem_3_117
      // Role: negated_conjecture
      List("topological_space(cx, ct)"),
      // Name: problem_3_118
      // Role: negated_conjecture
      List("subset_sets(cy, cx)"),
      // Name: problem_3_119
      // Role: negated_conjecture
      List("~topological_space(cy, subspace_topology(cx, ct, cy))"))
  
  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
