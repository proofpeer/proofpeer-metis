package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET004_MINUS1 {
  val mainClauses =
    List(
      // Name: a_union_b_is_aUb
      // Role: hypothesis
      List("Union(A, B, AUb)"),
      // Name: prove_a_is_a_subset_of_aUb
      // Role: negated_conjecture
      List("~Subset(A, AUb)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses++
      Axioms.SET001_MINUS1.clauses ++
      this.mainClauses
}
