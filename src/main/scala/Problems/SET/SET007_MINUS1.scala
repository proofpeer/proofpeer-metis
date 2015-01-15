package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET007_MINUS1 {
  val mainClauses =
    List(
      // Name: b_union_c
      // Role: axiom
      List("Union(B, C, BUc)"),
      // Name: a_intersection_b
      // Role: axiom
      List("Intersection(A, B, AIb)"),
      // Name: a_intersection_c
      // Role: axiom
      List("Intersection(A, C, AIc)"),
      // Name: a_intersection_bUc
      // Role: axiom
      List("Intersection(A, BUc, AI_bUc)"),
      // Name: prove_aIb_union_aIc_is_aI_bUc
      // Role: negated_conjecture
      List("~Union(AIb, AIc, AI_bUc)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses++
      Axioms.SET001_MINUS1.clauses++
      Axioms.SET001_MINUS2.clauses ++
      this.mainClauses
}
