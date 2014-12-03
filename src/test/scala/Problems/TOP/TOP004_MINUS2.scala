package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP004_MINUS2{
  val mainClauses =
    List(
      // Name: union_of_members_3
      // Role: axiom
      List("~element_of_collection(x1, x2)","~element_of_set(x0, x1)","element_of_set(x0, union_of_members(x2))"),
      // Name: basis_for_topology_28
      // Role: axiom
      List("~basis(x1, x0)","equal_sets(union_of_members(x0), x1)"),
      // Name: basis_for_topology_29
      // Role: axiom
      List("~basis(x3, x2)","~element_of_collection(x0, x2)","~element_of_collection(x1, x2)","~element_of_set(x4, x3)","~element_of_set(x4, intersection_of_sets(x0, x1))","element_of_set(x4, f6(x3, x2, x4, x0, x1))"),
      // Name: basis_for_topology_30
      // Role: axiom
      List("~basis(x3, x2)","~element_of_collection(x0, x2)","~element_of_collection(x1, x2)","~element_of_set(x4, x3)","~element_of_set(x4, intersection_of_sets(x0, x1))","element_of_collection(f6(x3, x2, x4, x0, x1), x2)"),
      // Name: basis_for_topology_31
      // Role: axiom
      List("~basis(x3, x2)","~element_of_collection(x0, x2)","~element_of_collection(x1, x2)","~element_of_set(x4, x3)","~element_of_set(x4, intersection_of_sets(x0, x1))","subset_sets(f6(x3, x2, x4, x0, x1), intersection_of_sets(x0, x1))"),
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
      // Name: set_theory_12
      // Role: axiom
      List("~subset_sets(x0, x1)","~subset_sets(x1, x2)","subset_sets(x0, x2)"),
      // Name: set_theory_13
      // Role: axiom
      List("~element_of_set(x2, intersection_of_sets(x0, x1))","element_of_set(x2, x0)"),
      // Name: set_theory_14
      // Role: axiom
      List("~element_of_set(x2, intersection_of_sets(x0, x1))","element_of_set(x2, x1)"),
      // Name: set_theory_15
      // Role: axiom
      List("~element_of_set(x2, x0)","~element_of_set(x2, x1)","element_of_set(x2, intersection_of_sets(x0, x1))"),
      // Name: set_theory_16
      // Role: axiom
      List("~subset_sets(x0, x1)","~subset_sets(x2, x3)","subset_sets(intersection_of_sets(x2, x0), intersection_of_sets(x3, x1))"),
      // Name: set_theory_17
      // Role: axiom
      List("~element_of_set(x2, x0)","~equal_sets(x0, x1)","element_of_set(x2, x1)"),
      // Name: set_theory_18
      // Role: axiom
      List("equal_sets(intersection_of_sets(x0, x1), intersection_of_sets(x1, x0))"),
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
     ++
      this.mainClauses
}
