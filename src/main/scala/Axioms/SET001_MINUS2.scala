package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET001_MINUS2 {
  val mainClauses =
    List(
      // Name: member_of_intersection_is_member_of_set1
      // Role: axiom
      List("~Intersection(x2, x3, x1)","~Member(x0, x1)","Member(x0, x2)"),
      // Name: member_of_intersection_is_member_of_set2
      // Role: axiom
      List("~Intersection(x2, x3, x1)","~Member(x0, x1)","Member(x0, x3)"),
      // Name: member_of_both_is_member_of_intersection
      // Role: axiom
      List("~Intersection(x2, x3, x1)","~Member(x0, x2)","~Member(x0, x3)","Member(x0, x1)"),
      // Name: intersection_axiom1
      // Role: axiom
      List("Intersection(x1, x2, x0)","Member(H(x1, x2, x0), x0)","Member(H(x1, x2, x0), x1)"),
      // Name: intersection_axiom2
      // Role: axiom
      List("Intersection(x1, x2, x0)","Member(H(x1, x2, x0), x0)","Member(H(x1, x2, x0), x2)"),
      // Name: intersection_axiom3
      // Role: axiom
      List("~Member(H(x1, x2, x0), x0)","~Member(H(x1, x2, x0), x1)","~Member(H(x1, x2, x0), x2)","Intersection(x1, x2, x0)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
