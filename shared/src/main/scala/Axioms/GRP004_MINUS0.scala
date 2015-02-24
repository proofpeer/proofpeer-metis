package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object GRP004_MINUS0 {
  val mainClauses =
    List(
      // Name: left_identity
      // Role: axiom
      List("(Multiply(Identity, x0) = x0)"),
      // Name: left_inverse
      // Role: axiom
      List("(Multiply(Inverse(x0), x0) = Identity)"),
      // Name: associativity
      // Role: axiom
      List("(Multiply(Multiply(x0, x1), x2) = Multiply(x0, Multiply(x1, x2)))")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
