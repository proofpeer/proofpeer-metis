package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.testing.tptp._

object TOP006_MINUS1 {
  val mainClauses =
    List(
      // Name: problem_1_110
      // Role: negated_conjecture
      List("basis(cx, ct)"),
      // Name: problem_1_111
      // Role: negated_conjecture
      List("~topological_space(cx, top_of_basis(ct))"))

  def clauses =
    Axioms.TOP001_MINUS0.mainClauses ++
      this.mainClauses
}
