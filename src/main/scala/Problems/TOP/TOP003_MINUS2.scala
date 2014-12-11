package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP003_MINUS2 {
  val mainClauses =
    List(
      // Name: union_of_members_1
      // Role: axiom
      List("~element_of_set(x0, union_of_members(x1))","element_of_set(x0, f1(x1, x0))"),
      // Name: union_of_members_2
      // Role: axiom
      List("~element_of_set(x0, union_of_members(x1))","element_of_collection(f1(x1, x0), x1)"),
      // Name: basis_for_topology_28
      // Role: axiom
      List("~basis(x1, x0)","equal_sets(union_of_members(x0), x1)"),
      // Name: topology_generated_40
      // Role: axiom
      List("element_of_collection(x0, top_of_basis(x1))","element_of_set(f11(x1, x0), x0)"),
      // Name: set_theory_7
      // Role: axiom
      List("~element_of_collection(x0, x1)","subset_sets(x0, union_of_members(x1))"),
      // Name: set_theory_8
      // Role: axiom
      List("~element_of_set(x0, x1)","~subset_sets(x1, x2)","element_of_set(x0, x2)"),
      // Name: set_theory_9
      // Role: axiom
      List("subset_sets(x0, x0)"),
      // Name: set_theory_10
      // Role: axiom
      List("~equal_sets(x0, x1)","~subset_sets(x2, x0)","subset_sets(x2, x1)"),
      // Name: set_theory_11
      // Role: axiom
      List("~equal_sets(x0, x1)","~subset_sets(x0, x2)","subset_sets(x1, x2)"),
      // Name: lemma_1c_1
      // Role: negated_conjecture
      List("basis(cx, f)"),
      // Name: lemma_1c_2
      // Role: negated_conjecture
      List("~element_of_collection(cx, top_of_basis(f))"))

  def clauses =
    this.mainClauses
}
