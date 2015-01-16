package proofpeer.metis.testing.tptp.Problems.SET

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET001_MINUS1 {
  val mainClauses =
    List(
      // Name: b_equals_bb
      // Role: hypothesis
      List("Equal_sets(B, Bb)"),
      // Name: element_of_b
      // Role: hypothesis
      List("Member(Element_of_b, B)"),
      // Name: prove_element_of_bb
      // Role: negated_conjecture
      List("~Member(Element_of_b, Bb)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET001_MINUS0.clauses ++
      this.mainClauses
}
