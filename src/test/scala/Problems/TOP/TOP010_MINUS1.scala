package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP010_MINUS1{
  val mainClauses =
    List(
      // Name: problem_5_123
      // Role: negated_conjecture
      List("finer(ct1, ct2, cx)"),
      // Name: problem_5_124
      // Role: negated_conjecture
      List("subset_sets(a, cx)"),
      // Name: problem_5_125
      // Role: negated_conjecture
      List("~finer(subspace_topology(cx, ct1, a), subspace_topology(cx, ct2, a), cx)"))
  
  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
