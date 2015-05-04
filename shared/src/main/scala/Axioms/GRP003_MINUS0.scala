package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object GRP003_MINUS0 {
  val mainClauses =
    List(
      // Name: left_identity
      // Role: axiom
      List("Product(Identity, x0, x0)"),
      // Name: right_identity
      // Role: axiom
      List("Product(x0, Identity, x0)"),
      // Name: left_inverse
      // Role: axiom
      List("Product(Inverse(x0), x0, Identity)"),
      // Name: right_inverse
      // Role: axiom
      List("Product(x0, Inverse(x0), Identity)"),
      // Name: total_function1
      // Role: axiom
      List("Product(x0, x1, Multiply(x0, x1))"),
      // Name: total_function2
      // Role: axiom
      List("~Product(x1, x2, x0)","~Product(x1, x2, x3)","(x3 = x0)"),
      // Name: associativity1
      // Role: axiom
      List("~Product(x0, x5, x2)","~Product(x3, x4, x0)","~Product(x4, x5, x1)","Product(x3, x1, x2)"),
      // Name: associativity2
      // Role: axiom
      List("~Product(x3, x1, x2)","~Product(x3, x4, x0)","~Product(x4, x5, x1)","Product(x0, x5, x2)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
