package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET010_MINUS1 {
  val mainClauses =
    List(
      // Name: a_intersection_b
      // Role: hypothesis
      List("Intersection(A, B, AIb)"),
      // Name: c_minus_a
      // Role: hypothesis
      List("Difference(C, A, CDa)"),
      // Name: c_minus_b
      // Role: hypothesis
      List("Difference(C, B, CDb)"),
      // Name: c_minus_aIb
      // Role: hypothesis
      List("Difference(C, AIb, CD_aIb)"),
      // Name: prove_cDa_union_cDb_is_cD_aIb
      // Role: negated_conjecture
      List("~Union(CDa, CDb, CD_aIb)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses++
      Axioms.SET001_MINUS1.clauses++
      Axioms.SET001_MINUS2.clauses++
      Axioms.SET001_MINUS3.clauses ++
      this.mainClauses
}
