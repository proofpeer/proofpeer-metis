package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP002_MINUS1 {
  val mainClauses =
    List(
      // Name: lemma_1b_1
      // Role: negated_conjecture
      List("basis(cx, f)"),
      // Name: lemma_1b_2
      // Role: negated_conjecture
      List("~element_of_collection(empty_set, top_of_basis(f))"))

  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
