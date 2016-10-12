package proofpeer.metis.fol.sttzf

import org.ensime.sexp.{ SexpCompactPrinter => SexpPrinter }

import scalaz._
import Scalaz._

import proofpeer.metis.fol.SExpr._
import ZFProver._

object Theory {
  val Success(Some(empty)) =
    interactive(
      "(ex Empty (! x (not (vin x Empty))))",
      List("((x) (not (= x x)))"),
      List()).map { _ >>= { define(_) }}
  val Success(Some(upair_def)) =
    interactive(
      "(! a (! b (ex UPair (! x (<-> (vin x UPair) (or (= x a) (= x b)))))))",
      List(),
      List(ZFProver.upair)).map { _ >>= { define(_) }}
  val Success(Some(eq_upair)) =
    interactive(
      "(<-> (= P (UPair a b)) (! x (<-> (vin x P) (or (= x a) (= x b)))))",
      List(),
      List(extensionality,upair_def))
  val Success(Some(vpair_def)) = interactive(
    """(! a (! b (ex VPair (= VPair (UPair (UPair (a a) (UPair a b)))))))""",
    List(),
    List()).map { _ >>= { define(_) }}
  val jam = interactive(
    "(<=> (= (vpair a b) (vpair c d)) (and (= a c) (= b d)))",
    List(),
    List(vpair_def, extensionality,upair_def))

  def showTheorem(thm: Theorem) =
    SexpPrinter(SExprOfFol(thm.thm,true))
}
