package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP013_MINUS1{
  val mainClauses =
    List(
      // Name: problem_8_139
      // Role: negated_conjecture
      List("topological_space(cx, ct)"),
      // Name: problem_8_140
      // Role: negated_conjecture
      List("subset_sets(a, cx)"),
      // Name: problem_8_141
      // Role: negated_conjecture
      List("~subset_sets(a, closure(a, cx, ct))","~subset_sets(interior(a, cx, ct), a)"))
  
  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
