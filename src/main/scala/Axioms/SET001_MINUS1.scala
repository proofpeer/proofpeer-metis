package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET001_MINUS1 {
  val mainClauses =
    List(
      // Name: member_of_union_is_member_of_one_set
      // Role: axiom
      List("~Member(x0, x3)","~Union(x1, x2, x3)","Member(x0, x1)","Member(x0, x2)"),
      // Name: member_of_set1_is_member_of_union
      // Role: axiom
      List("~Member(x0, x1)","~Union(x1, x2, x3)","Member(x0, x3)"),
      // Name: member_of_set2_is_member_of_union
      // Role: axiom
      List("~Member(x0, x2)","~Union(x1, x2, x3)","Member(x0, x3)"),
      // Name: union_axiom1
      // Role: axiom
      List("Member(G(x0, x1, x2), x0)","Member(G(x0, x1, x2), x1)","Member(G(x0, x1, x2), x2)","Union(x0, x1, x2)"),
      // Name: union_axiom2
      // Role: axiom
      List("~Member(G(x0, x1, x2), x0)","~Member(G(x0, x1, x2), x2)","Union(x0, x1, x2)"),
      // Name: union_axiom3
      // Role: axiom
      List("~Member(G(x0, x1, x2), x1)","~Member(G(x0, x1, x2), x2)","Union(x0, x1, x2)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
