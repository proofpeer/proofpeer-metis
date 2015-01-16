package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET003_MINUS1 {
  val mainClauses =
    List(
      // Name: a_union_a_is_aUa
      // Role: hypothesis
      List("Union(A, A, AUa)"),
      // Name: prove_a_is_a_subset_of_aUa
      // Role: negated_conjecture
      List("~Subset(A, AUa)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses++
      Axioms.SET001_MINUS1.clauses ++
      this.mainClauses
}
