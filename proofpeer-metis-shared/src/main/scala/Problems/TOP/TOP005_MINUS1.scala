package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP005_MINUS1 {
  val mainClauses =
    List(
      // Name: lemma_1e_1
      // Role: negated_conjecture
      List("basis(cx, f)"),
      // Name: lemma_1e_2
      // Role: negated_conjecture
      List("subset_collections(g, top_of_basis(f))"),
      // Name: lemma_1e_3
      // Role: negated_conjecture
      List("~element_of_collection(union_of_members(g), top_of_basis(f))"))

  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
