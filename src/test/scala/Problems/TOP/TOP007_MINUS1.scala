package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP007_MINUS1{
  val mainClauses =
    List(
      // Name: problem_2_112
      // Role: negated_conjecture
      List("topological_space(cx, ct)"),
      // Name: problem_2_113
      // Role: negated_conjecture
      List("subset_sets(a, cx)"),
      // Name: problem_2_114
      // Role: negated_conjecture
      List("~element_of_set(x0, a)","neighborhood(f30(x0), x0, cx, ct)"),
      // Name: problem_2_115
      // Role: negated_conjecture
      List("~element_of_set(x0, a)","subset_sets(f30(x0), a)"),
      // Name: problem_2_116
      // Role: negated_conjecture
      List("~open(a, cx, ct)"))
  
  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
