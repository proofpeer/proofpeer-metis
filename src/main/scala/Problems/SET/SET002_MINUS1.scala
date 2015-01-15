package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET002_MINUS1 {
  val mainClauses =
    List(
      // Name: a_union_a_is_aUa
      // Role: hypothesis
      List("Union(A, A, AUa)"),
      // Name: prove_a_equals_aUa
      // Role: negated_conjecture
      List("~Equal_sets(AUa, A)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses++
      Axioms.SET001_MINUS1.clauses ++
      this.mainClauses
}
