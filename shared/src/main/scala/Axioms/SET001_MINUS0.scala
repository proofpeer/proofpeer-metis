package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET001_MINUS0 {
  val mainClauses =
    List(
      // Name: membership_in_subsets
      // Role: axiom
      List("~Member(x0, x1)","~Subset(x1, x2)","Member(x0, x2)"),
      // Name: subsets_axiom1
      // Role: axiom
      List("Member(Member_of_1_not_of_2(x0, x1), x0)","Subset(x0, x1)"),
      // Name: subsets_axiom2
      // Role: axiom
      List("~Member(Member_of_1_not_of_2(x0, x1), x1)","Subset(x0, x1)"),
      // Name: set_equal_sets_are_subsets1
      // Role: axiom
      List("~Equal_sets(x0, x1)","Subset(x0, x1)"),
      // Name: set_equal_sets_are_subsets2
      // Role: axiom
      List("~Equal_sets(x1, x0)","Subset(x0, x1)"),
      // Name: subsets_are_set_equal_sets
      // Role: axiom
      List("~Subset(x0, x1)","~Subset(x1, x0)","Equal_sets(x1, x0)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
