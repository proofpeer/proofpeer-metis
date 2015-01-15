package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET006_MINUS1 {
  val mainClauses =
    List(
      // Name: d_intersection_a_is_d
      // Role: hypothesis
      List("Intersection(D, A, D)"),
      // Name: prove_d_is_a_subset_of_a
      // Role: negated_conjecture
      List("~Subset(D, A)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses++
      Axioms.SET001_MINUS2.clauses ++
      this.mainClauses
}
