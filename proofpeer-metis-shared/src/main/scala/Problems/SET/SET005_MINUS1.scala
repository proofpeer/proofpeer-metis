package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET005_MINUS1 {
  val mainClauses =
    List(
      // Name: a_intersection_b
      // Role: axiom
      List("Intersection(A, B, AIb)"),
      // Name: b_intersection_c
      // Role: axiom
      List("Intersection(B, C, BIc)"),
      // Name: a_intersection_bIc
      // Role: axiom
      List("Intersection(A, BIc, AIbIc)"),
      // Name: prove_aIb_intersection_c_is_aIbIc
      // Role: negated_conjecture
      List("~Intersection(AIb, C, AIbIc)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses++
      Axioms.SET001_MINUS2.clauses ++
      this.mainClauses
}
