package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP004_MINUS1{
  val mainClauses =
    List(
      // Name: lemma_1d_1
      // Role: negated_conjecture
      List("basis(cx, f)"),
      // Name: lemma_1d_2
      // Role: negated_conjecture
      List("element_of_collection(x0, top_of_basis(f))"),
      // Name: lemma_1d_3
      // Role: negated_conjecture
      List("element_of_collection(x0, top_of_basis(f))"),
      // Name: lemma_1d_4
      // Role: negated_conjecture
      List("~element_of_collection(intersection_of_sets(x0, x1), top_of_basis(f))"))
  
  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
