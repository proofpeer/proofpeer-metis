package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP001_MINUS2{
  val mainClauses =
    List(
      // Name: union_of_members_1
      // Role: axiom
      List("~element_of_set(x0, union_of_members(x1))","element_of_set(x0, f1(x1, x0))"),
      // Name: union_of_members_2
      // Role: axiom
      List("~element_of_set(x0, union_of_members(x1))","element_of_collection(f1(x1, x0), x1)"),
      // Name: union_of_members_3
      // Role: axiom
      List("~element_of_collection(x1, x2)","~element_of_set(x0, x1)","element_of_set(x0, union_of_members(x2))"),
      // Name: basis_for_topology_28
      // Role: axiom
      List("~basis(x1, x0)","equal_sets(union_of_members(x0), x1)"),
      // Name: topology_generated_37
      // Role: axiom
      List("~element_of_collection(x0, top_of_basis(x1))","~element_of_set(x2, x0)","element_of_set(x2, f10(x1, x0, x2))"),
      // Name: topology_generated_38
      // Role: axiom
      List("~element_of_collection(x0, top_of_basis(x1))","~element_of_set(x2, x0)","element_of_collection(f10(x1, x0, x2), x1)"),
      // Name: set_theory_1
      // Role: axiom
      List("subset_sets(x0, x0)"),
      // Name: set_theory_2
      // Role: axiom
      List("~element_of_set(x0, x1)","~subset_sets(x1, x2)","element_of_set(x0, x2)"),
      // Name: set_theory_3
      // Role: axiom
      List("~equal_sets(x0, x1)","subset_sets(x0, x1)"),
      // Name: set_theory_4
      // Role: axiom
      List("element_of_set(in_1st_set(x0, x1), x0)","subset_sets(x0, x1)"),
      // Name: set_theory_5
      // Role: axiom
      List("~element_of_set(in_1st_set(x0, x1), x1)","subset_sets(x0, x1)"),
      // Name: lemma_1a_1
      // Role: negated_conjecture
      List("basis(cx, f)"),
      // Name: lemma_1a_2
      // Role: negated_conjecture
      List("~subset_sets(union_of_members(top_of_basis(f)), cx)"))
  
  def clauses =
     ++
      this.mainClauses
}
