package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET008_MINUS1 {
  val mainClauses =
    List(
      // Name: b_minus_a
      // Role: hypothesis
      List("Difference(B, A, BDa)"),
      // Name: a_intersection_bDa
      // Role: negated_conjecture
      List("~Intersection(A, BDa, AI_bDa)"),
      // Name: prove_aI_bDa_is_empty
      // Role: negated_conjecture
      List("~Member(x0, AI_bDa)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses++
      Axioms.SET001_MINUS2.clauses++
      Axioms.SET001_MINUS3.clauses ++
      this.mainClauses
}
