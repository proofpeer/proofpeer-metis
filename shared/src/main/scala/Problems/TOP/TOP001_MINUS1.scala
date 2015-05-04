package proofpeer.metis.testing.tptp.Problems.TOP

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object TOP001_MINUS1 {
  val mainClauses =
    List(
      // Name: lemma_1a_1
      // Role: negated_conjecture
      List("Basis(Cx, F)"),
      // Name: lemma_1a_2
      // Role: negated_conjecture
      List("~Subset_sets(Union_of_members(Top_of_basis(F)), Cx)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.TOP001_MINUS0.clauses ++
      this.mainClauses
}
