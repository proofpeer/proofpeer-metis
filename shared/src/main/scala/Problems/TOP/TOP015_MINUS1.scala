package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP015_MINUS1 {
  val mainClauses =
    List(
      // Name: problem_10_148
      // Role: negated_conjecture
      List("topological_space(cx, ct)"),
      // Name: problem_10_149
      // Role: negated_conjecture
      List("subset_sets(a, cx)"),
      // Name: problem_10_150
      // Role: negated_conjecture
      List("~equal_sets(intersection_of_sets(interior(a, cx, ct), boundary(a, cx, ct)), empty_set)"))

  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
