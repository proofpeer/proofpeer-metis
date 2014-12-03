package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP012_MINUS1{
  val mainClauses =
    List(
      // Name: problem_7_129
      // Role: negated_conjecture
      List("topological_space(cx, ct)"),
      // Name: problem_7_130
      // Role: negated_conjecture
      List("~closed(cx, cx, ct)","~closed(empty_set, cx, ct)","closed(cy1, cx, ct)","subset_sets(union_of_members(f), cx)"),
      // Name: problem_7_131
      // Role: negated_conjecture
      List("~closed(cx, cx, ct)","~closed(empty_set, cx, ct)","~element_of_collection(x0, f)","closed(x0, cx, ct)","closed(cy1, cx, ct)"),
      // Name: problem_7_132
      // Role: negated_conjecture
      List("~closed(cx, cx, ct)","~closed(empty_set, cx, ct)","~closed(intersection_of_members(f), cx, ct)","closed(cy1, cx, ct)"),
      // Name: problem_7_133
      // Role: negated_conjecture
      List("~closed(cx, cx, ct)","~closed(empty_set, cx, ct)","closed(cy2, cx, ct)","subset_sets(union_of_members(f), cx)"),
      // Name: problem_7_134
      // Role: negated_conjecture
      List("~closed(cx, cx, ct)","~closed(empty_set, cx, ct)","~element_of_collection(x0, f)","closed(x0, cx, ct)","closed(cy2, cx, ct)"),
      // Name: problem_7_135
      // Role: negated_conjecture
      List("~closed(cx, cx, ct)","~closed(empty_set, cx, ct)","~closed(intersection_of_members(f), cx, ct)","closed(cy2, cx, ct)"),
      // Name: problem_7_136
      // Role: negated_conjecture
      List("~closed(cx, cx, ct)","~closed(empty_set, cx, ct)","~closed(union_of_sets(cy1, cy2), cx, ct)","subset_sets(union_of_members(f), cx)"),
      // Name: problem_7_137
      // Role: negated_conjecture
      List("~closed(cx, cx, ct)","~closed(empty_set, cx, ct)","~closed(union_of_sets(cy1, cy2), cx, ct)","~element_of_collection(x0, f)","closed(x0, cx, ct)"),
      // Name: problem_7_138
      // Role: negated_conjecture
      List("~closed(cx, cx, ct)","~closed(empty_set, cx, ct)","~closed(intersection_of_members(f), cx, ct)","~closed(union_of_sets(cy1, cy2), cx, ct)"))
  
  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
