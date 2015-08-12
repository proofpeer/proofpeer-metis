package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET009_MINUS1 {
  val mainClauses =
    List(
      // Name: d_is_a_subset_of_a
      // Role: hypothesis
      List("Subset(D, A)"),
      // Name: b_minus_a
      // Role: hypothesis
      List("Difference(B, A, BDa)"),
      // Name: b_minus_d
      // Role: hypothesis
      List("Difference(B, D, BDd)"),
      // Name: prove_bDa_is_a_subset_of_bDd
      // Role: negated_conjecture
      List("~Subset(BDa, BDd)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses++
      Axioms.SET001_MINUS3.clauses ++
      this.mainClauses
}
