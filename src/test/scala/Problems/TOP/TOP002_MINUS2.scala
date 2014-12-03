package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP002_MINUS2{
  val mainClauses =
    List(
      // Name: topology_generated_40
      // Role: axiom
      List("element_of_collection(x0, top_of_basis(x1))","element_of_set(f11(x1, x0), x0)"),
      // Name: set_theory_6
      // Role: axiom
      List("~element_of_set(x0, empty_set)"),
      // Name: lemma_1b_2
      // Role: negated_conjecture
      List("~element_of_collection(empty_set, top_of_basis(f))"))
  
  def clauses =
     ++
      this.mainClauses
}
