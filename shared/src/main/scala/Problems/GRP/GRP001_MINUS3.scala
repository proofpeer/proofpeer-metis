package proofpeer.metis.testing.tptp.Problems.GRP

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object GRP001_MINUS3 {
  val mainClauses =
    List(
      // Name: a_group
      // Role: hypothesis
      List("Group(F71, F72)"),
      // Name: f73_is_the_identity
      // Role: hypothesis
      List("Identity(F71, F72, F73)"),
      // Name: x_squared_is_identity
      // Role: hypothesis
      List("~Member(x0, F71)","(Apply_to_two_arguments(F72, x0, x0) = F73)"),
      // Name: prove_the_group_is_commutative
      // Role: negated_conjecture
      List("~Commutes(F71, F72)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.SET003_MINUS0.clauses++
      Axioms.ALG001_MINUS0.clauses ++
      this.mainClauses
}
