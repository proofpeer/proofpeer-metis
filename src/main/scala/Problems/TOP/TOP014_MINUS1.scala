package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP014_MINUS1 {
  val mainClauses =
    List(
      // Name: problem_9_142
      // Role: negated_conjecture
      List("topological_space(cx, ct)"),
      // Name: problem_9_143
      // Role: negated_conjecture
      List("subset_sets(a, cx)"),
      // Name: problem_9_144
      // Role: negated_conjecture
      List("closed(a, cx, ct)","equal_sets(a, closure(a, cx, ct))","equal_sets(a, interior(a, cx, ct))","open(a, cx, ct)"),
      // Name: problem_9_145
      // Role: negated_conjecture
      List("~closed(a, cx, ct)","~equal_sets(a, closure(a, cx, ct))","equal_sets(a, interior(a, cx, ct))","open(a, cx, ct)"),
      // Name: problem_9_146
      // Role: negated_conjecture
      List("~equal_sets(a, interior(a, cx, ct))","~open(a, cx, ct)","closed(a, cx, ct)","equal_sets(a, closure(a, cx, ct))"),
      // Name: problem_9_147
      // Role: negated_conjecture
      List("~closed(a, cx, ct)","~equal_sets(a, closure(a, cx, ct))","~equal_sets(a, interior(a, cx, ct))","~open(a, cx, ct)"))

  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
