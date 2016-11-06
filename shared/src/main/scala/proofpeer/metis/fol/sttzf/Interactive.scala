package proofpeer.metis.fol.sttzf

import org.ensime.sexp.{ SexpParser, Sexp, SexpSymbol,
  SexpCompactPrinter => SexpPrinter }

import scalaz._
import Scalaz._

import proofpeer.metis.Sexp.{ SexpFn, functorOfString }
import proofpeer.metis.fol.{ Sexp, FOL  }
import proofpeer.metis.fol.Sexp.SexpOfFol

import ZFProver._

object Interactive {
  def sepOfString(x: String, sepBody: String): Theorem = {
    val body = folOfString(sepBody)
    separationInst(functorOfString(x), body)
  }

  def folFrees(fol: FOLSexpFn): (ISet[SexpFn], ISet[SexpFn], ISet[SexpFn]) =
    FOL.trifoldMap[
      SexpFn,SexpFn,SexpFn,FOL.Neg,FOL.Binder,
      (ISet[SexpFn],ISet[SexpFn],ISet[SexpFn])](fol)(
        v => (ISet.singleton(v),ISet.empty,ISet.empty),
        f => (ISet.empty,ISet.singleton(f),ISet.empty),
        p => (ISet.empty,ISet.empty,ISet.singleton(p)))

   def interactive(
     conjecture: String,
     sepInsts: List[(String, String)],
     lemmas: List[Theorem],
     fdefns: List[SexpFn ==>> FOLSexpFn]) = {

     val theConjecture = folOfString(conjecture)
     val vs = theConjecture.frees
     val (_,fs,_) = folFrees(theConjecture)
     vs.toList.foreach { v =>
       System.out.println("Note: " + v + " is free.")
     }
     val lhss = lemmas.foldMap(_.definitions.keySet)
       .insert(functorOfString("="))
       .insert(functorOfString("vin"))
       .insert(functorOfString("member"))
     fs.toList.foreach { f =>
       if (!lhss.member(f))
         System.out.println("Note: " + f + " is undefined")
     }
     val lemmaSeps =
       sepInsts.map { case (x,body) => sepOfString(x,body) } ++ lemmas
     prove(theConjecture, lemmaSeps, fdefns)
   }
}
