package proofpeer.metis.testing.tptp.Problems.GRP

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object GRP001_MINUS4 {
  val mainClauses =
    List(
      // Name: associativity
      // Role: axiom
      List("(Multiply(Multiply(x0, x1), x2) = Multiply(x0, Multiply(x1, x2)))"),
      // Name: left_identity
      // Role: axiom
      List("(Multiply(Identity, x0) = x0)"),
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
    this.mainClauses
}
