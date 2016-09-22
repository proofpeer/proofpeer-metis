package proofpeer.metis.fol.sttzf

import org.ensime.sexp.SexpParser
import org.ensime.sexp.Sexp
import scalaz._
import Scalaz._

import proofpeer.metis.fol._

object Axioms {
  def folOfString(str: String):
      ValidationNel[(Sexp,String),FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]] =
    SExpr.folOfSExpr(SexpParser.parse(str),true)

  val extensionality =
    folOfString("(<-> ((=) a b) (! x (<-> ((vin) x A) ((vin) x B))))")
  val upair =
    folOfString("(ex P (! x (<-> ((vin) x P) (or ((=) x a) ((=) y a)))))")
  val funion =
    folOfString("(ex U (! x (<-> ((vin) x P) (or ((vin) x A) ((vin) x B)))))")
  val powerset =
    folOfString("(ex P (! X (<-> ((vin) X P) (! x (-> ((vin) x X) ((vin) x A))))))")
}
