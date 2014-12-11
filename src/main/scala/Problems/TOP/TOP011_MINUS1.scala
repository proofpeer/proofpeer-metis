package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP011_MINUS1 {
  val mainClauses =
    List(
      // Name: problem_6_126
      // Role: negated_conjecture
      List("element_of_set(cu, top_of_basis(f))","subset_collections(g, f)"),
      // Name: problem_6_127
      // Role: negated_conjecture
      List("element_of_set(cu, top_of_basis(f))","equal_sets(cu, union_of_members(g))"),
      // Name: problem_6_128
      // Role: negated_conjecture
      List("~element_of_set(cu, top_of_basis(f))","~equal_sets(cu, union_of_members(x0))","~subset_collections(x0, f)"))

  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
