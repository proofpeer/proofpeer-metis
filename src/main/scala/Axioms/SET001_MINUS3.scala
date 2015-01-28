package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET001_MINUS3 {
  val mainClauses =
    List(
      // Name: member_of_difference
      // Role: axiom
      List("~Difference(x2, x3, x0)","~Member(x1, x0)","Member(x1, x2)"),
      // Name: not_member_of_difference
      // Role: axiom
      List("~Difference(x0, x2, x3)","~Member(x1, x2)","~Member(x1, x3)"),
      // Name: member_of_difference_or_set2
      // Role: axiom
      List("~Difference(x2, x3, x0)","~Member(x1, x2)","Member(x1, x0)","Member(x1, x3)"),
      // Name: difference_axiom2
      // Role: axiom
      List("Difference(x1, x2, x0)","Member(K(x1, x2, x0), x0)","Member(K(x1, x2, x0), x1)"),
      // Name: difference_axiom1
      // Role: axiom
      List("~Member(K(x1, x2, x0), x2)","Difference(x1, x2, x0)","Member(K(x1, x2, x0), x0)"),
      // Name: difference_axiom3
      // Role: axiom
      List("~Member(K(x1, x2, x0), x0)","~Member(K(x1, x2, x0), x1)","Difference(x1, x2, x0)","Member(K(x1, x2, x0), x2)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
