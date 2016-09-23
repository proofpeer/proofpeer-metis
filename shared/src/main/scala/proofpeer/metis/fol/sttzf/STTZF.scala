package proofpeer.metis.fol.sttzf

import org.ensime.sexp.SexpParser
import scalaz._
import Scalaz._
import org.ensime.sexp.{ Sexp, SexpCons, SexpNumber }

import proofpeer.metis.Term
import proofpeer.metis.fol._

object Axioms {
  def folOfString(str: String):
      ValidationNel[(Sexp,String),FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]] =
    SExpr.folOfSExpr(SexpParser.parse(str),true)

  val extensionality =
    folOfString("(<-> (= a b) (! x (<-> (vin x A) (vin x B))))")
  val upair =
    folOfString("(ex P (! x (<-> (vin x P) (or (= x a) (= y a)))))")
  val funion =
    folOfString("(ex U (! x (<-> (vin x P) (or (vin x A) (vin x B)))))")
  val powerset =
    folOfString("(ex P (! X (<-> ((vin) X P) (! x (-> ((vin) x X) ((vin) x A))))))")
  def separation(
    inst: (Sexp, List[Term[Sexp,Sexp]]) => FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]) =
    folOfString("(ex S (! x (<-> (vin x S) (and (vin x A) (P x)))))").map {
      fol => FOL.instPred(fol)(inst)
    }
}
