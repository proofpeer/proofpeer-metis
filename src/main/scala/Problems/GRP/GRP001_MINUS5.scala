package proofpeer.metis.testing.tptp.Problems.GRP

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object GRP001_MINUS5 {
  val mainClauses =
    List(
      // Name: left_identity
      // Role: axiom
      List("Product(Identity, x0, x0)"),
      // Name: right_identity
      // Role: axiom
      List("Product(x0, Identity, x0)"),
      // Name: associativity1
      // Role: axiom
      List("~Product(x0, x5, x2)","~Product(x3, x4, x0)","~Product(x4, x5, x1)","Product(x3, x1, x2)"),
      // Name: associativity2
      // Role: axiom
      List("~Product(x3, x1, x2)","~Product(x3, x4, x0)","~Product(x4, x5, x1)","Product(x0, x5, x2)"),
      // Name: square_element
      // Role: hypothesis
      List("Product(x0, x0, Identity)"),
      // Name: a_times_b_is_c
      // Role: hypothesis
      List("Product(A, B, C)"),
      // Name: prove_b_times_a_is_c
      // Role: negated_conjecture
      List("~Product(B, A, C)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
