package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP005_MINUS2 {
  val mainClauses =
    List(
      // Name: union_of_members_1
      // Role: axiom
      List("~element_of_set(x0, union_of_members(x1))","element_of_set(x0, f1(x1, x0))"),
      // Name: union_of_members_2
      // Role: axiom
      List("~element_of_set(x0, union_of_members(x1))","element_of_collection(f1(x1, x0), x1)"),
      // Name: topology_generated_37
      // Role: axiom
      List("~element_of_collection(x0, top_of_basis(x1))","~element_of_set(x2, x0)","element_of_set(x2, f10(x1, x0, x2))"),
      // Name: topology_generated_38
      // Role: axiom
      List("~element_of_collection(x0, top_of_basis(x1))","~element_of_set(x2, x0)","element_of_collection(f10(x1, x0, x2), x1)"),
      // Name: topology_generated_39
      // Role: axiom
      List("~element_of_collection(x0, top_of_basis(x1))","~element_of_set(x2, x0)","subset_sets(f10(x1, x0, x2), x0)"),
      // Name: topology_generated_40
      // Role: axiom
      List("element_of_collection(x0, top_of_basis(x1))","element_of_set(f11(x1, x0), x0)"),
      // Name: topology_generated_41
      // Role: axiom
      List("~element_of_collection(x1, x2)","~element_of_set(f11(x2, x0), x1)","~subset_sets(x1, x0)","element_of_collection(x0, top_of_basis(x2))"),
      // Name: set_theory_19
      // Role: axiom
      List("~element_of_set(x0, x1)","element_of_set(x0, x2)","subset_sets(x1, x2)"),
      // Name: set_theory_20
      // Role: axiom
      List("~element_of_collection(x1, x2)","~subset_sets(x0, x1)","subset_sets(x0, union_of_members(x2))"),
      // Name: set_theory_21
      // Role: axiom
      List("~element_of_collection(x0, x1)","~subset_collections(x1, x2)","element_of_collection(x0, x2)"),
      // Name: lemma_1e_2
      // Role: negated_conjecture
      List("subset_collections(g, top_of_basis(f))"),
      // Name: lemma_1e_3
      // Role: negated_conjecture
      List("~element_of_collection(union_of_members(g), top_of_basis(f))"))

  def clauses =
    this.mainClauses
}
