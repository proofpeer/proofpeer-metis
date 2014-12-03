package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP001_MINUS1{
  val mainClauses =
    List(
      // Name: lemma_1a_1
      // Role: negated_conjecture
      List("basis(cx, f)"),
      // Name: lemma_1a_2
      // Role: negated_conjecture
      List("~subset_sets(union_of_members(top_of_basis(f)), cx)"))
  
  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
