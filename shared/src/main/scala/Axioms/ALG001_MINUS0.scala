package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object ALG001_MINUS0 {
  val mainClauses =
    List(
      // Name: associative_system1
      // Role: axiom
      List("~Associative(x2, x1)","~Member(x0, x2)","~Member(x3, x2)","~Member(x4, x2)","(Apply_to_two_arguments(x1, Apply_to_two_arguments(x1, x0, x3), x4) = Apply_to_two_arguments(x1, x0, Apply_to_two_arguments(x1, x3, x4)))"),
      // Name: associative_system2
      // Role: axiom
      List("Associative(x1, x0)","Member(F34(x1, x0), x1)"),
      // Name: associative_system3
      // Role: axiom
      List("Associative(x1, x0)","Member(F35(x1, x0), x1)"),
      // Name: associative_system4
      // Role: axiom
      List("Associative(x1, x0)","Member(F36(x1, x0), x1)"),
      // Name: associative_system5
      // Role: axiom
      List("~(Apply_to_two_arguments(x0, Apply_to_two_arguments(x0, F34(x1, x0), F35(x1, x0)), F36(x1, x0)) = Apply_to_two_arguments(x0, F34(x1, x0), Apply_to_two_arguments(x0, F35(x1, x0), F36(x1, x0))))","Associative(x1, x0)"),
      // Name: identity1
      // Role: axiom
      List("~Identity(x2, x1, x0)","Member(x0, x2)"),
      // Name: identity2
      // Role: axiom
      List("~Identity(x3, x2, x1)","~Member(x0, x3)","(Apply_to_two_arguments(x2, x1, x0) = x0)"),
      // Name: identity3
      // Role: axiom
      List("~Identity(x3, x2, x1)","~Member(x0, x3)","(Apply_to_two_arguments(x2, x0, x1) = x0)"),
      // Name: identity4
      // Role: axiom
      List("~Member(x0, x2)","Identity(x2, x1, x0)","Member(F37(x2, x1, x0), x2)"),
      // Name: identity5
      // Role: axiom
      List("~(Apply_to_two_arguments(x1, x0, F37(x2, x1, x0)) = F37(x2, x1, x0))","~(Apply_to_two_arguments(x1, F37(x2, x1, x0), x0) = F37(x2, x1, x0))","~Member(x0, x2)","Identity(x2, x1, x0)"),
      // Name: inverse1
      // Role: axiom
      List("~Inverse(x3, x1, x0, x2)","Maps(x2, x3, x3)"),
      // Name: inverse2
      // Role: axiom
      List("~Inverse(x4, x2, x1, x3)","~Member(x0, x4)","(Apply_to_two_arguments(x2, Apply(x3, x0), x0) = x1)"),
      // Name: inverse3
      // Role: axiom
      List("~Inverse(x4, x2, x1, x3)","~Member(x0, x4)","(Apply_to_two_arguments(x2, x0, Apply(x3, x0)) = x1)"),
      // Name: inverse4
      // Role: axiom
      List("~Maps(x2, x3, x3)","Inverse(x3, x1, x0, x2)","Member(F38(x3, x1, x0, x2), x3)"),
      // Name: inverse5
      // Role: axiom
      List("~(Apply_to_two_arguments(x1, Apply(x2, F38(x3, x1, x0, x2)), F38(x3, x1, x0, x2)) = x0)","~(Apply_to_two_arguments(x1, F38(x3, x1, x0, x2), Apply(x2, F38(x3, x1, x0, x2))) = x0)","~Maps(x2, x3, x3)","Inverse(x3, x1, x0, x2)"),
      // Name: group1
      // Role: axiom
      List("~Group(x1, x0)","Closed(x1, x0)"),
      // Name: group2
      // Role: axiom
      List("~Group(x1, x0)","Associative(x1, x0)"),
      // Name: group3
      // Role: axiom
      List("~Group(x1, x0)","Identity(x1, x0, F39(x1, x0))"),
      // Name: group4
      // Role: axiom
      List("~Group(x1, x0)","Inverse(x1, x0, F39(x1, x0), F40(x1, x0))"),
      // Name: group5
      // Role: axiom
      List("~Associative(x3, x1)","~Closed(x3, x1)","~Identity(x3, x1, x0)","~Inverse(x3, x1, x0, x2)","Group(x3, x1)"),
      // Name: commutes1
      // Role: axiom
      List("~Commutes(x2, x1)","~Member(x0, x2)","~Member(x3, x2)","(Apply_to_two_arguments(x1, x0, x3) = Apply_to_two_arguments(x1, x3, x0))"),
      // Name: commutes2
      // Role: axiom
      List("Commutes(x1, x0)","Member(F41(x1, x0), x1)"),
      // Name: commutes3
      // Role: axiom
      List("Commutes(x1, x0)","Member(F42(x1, x0), x1)"),
      // Name: commutes4
      // Role: axiom
      List("~(Apply_to_two_arguments(x0, F41(x1, x0), F42(x1, x0)) = Apply_to_two_arguments(x0, F42(x1, x0), F41(x1, x0)))","Commutes(x1, x0)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
