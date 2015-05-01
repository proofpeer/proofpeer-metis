package proofpeer.metis.testing.tptp.Problems.GRP

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object GRP001_MINUS1 {
  val mainClauses =
    List(
      // Name: square_element
      // Role: hypothesis
      List("Product(x0, x0, Identity)"),
      // Name: a_times_b_is_c
      // Role: negated_conjecture
      List("Product(A, B, C)"),
      // Name: prove_b_times_a_is_c
      // Role: negated_conjecture
      List("~Product(B, A, C)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.GRP003_MINUS0.clauses ++
      this.mainClauses
}
