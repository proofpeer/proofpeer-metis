package proofpeer.metis.testing.tptp.Problems.GRP

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object GRP001_MINUS2 {
  val mainClauses =
    List(
      // Name: right_identity
      // Role: axiom
      List("(Multiply(x0, Identity) = x0)"),
      // Name: right_inverse
      // Role: axiom
      List("(Multiply(x0, Inverse(x0)) = Identity)"),
      // Name: squareness
      // Role: hypothesis
      List("(Multiply(x0, x0) = Identity)"),
      // Name: a_times_b_is_c
      // Role: hypothesis
      List("(Multiply(A, B) = C)"),
      // Name: prove_b_times_a_is_c
      // Role: negated_conjecture
      List("~(Multiply(B, A) = C)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.GRP004_MINUS0.clauses ++
      this.mainClauses
}
