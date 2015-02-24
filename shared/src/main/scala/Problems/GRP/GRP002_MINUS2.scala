package proofpeer.metis.testing.tptp.Problems.GRP

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object GRP002_MINUS2 {
  val mainClauses =
    List(
      // Name: right_identity
      // Role: axiom
      List("(Multiply(x0, Identity) = x0)"),
      // Name: right_inverse
      // Role: axiom
      List("(Multiply(x0, Inverse(x0)) = Identity)"),
      // Name: x_cubed_is_identity
      // Role: hypothesis
      List("(Multiply(x0, Multiply(x0, x0)) = Identity)"),
      // Name: a_times_b_is_c
      // Role: negated_conjecture
      List("(Multiply(A, B) = C)"),
      // Name: c_times_inverse_a_is_d
      // Role: negated_conjecture
      List("(Multiply(C, Inverse(A)) = D)"),
      // Name: d_times_inverse_b_is_h
      // Role: negated_conjecture
      List("(Multiply(D, Inverse(B)) = H)"),
      // Name: h_times_b_is_j
      // Role: negated_conjecture
      List("(Multiply(H, B) = J)"),
      // Name: j_times_inverse_h_is_k
      // Role: negated_conjecture
      List("(Multiply(J, Inverse(H)) = K)"),
      // Name: prove_k_times_inverse_b_is_e
      // Role: negated_conjecture
      List("~(Multiply(K, Inverse(B)) = Identity)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    Axioms.GRP004_MINUS0.clauses ++
      this.mainClauses
}
