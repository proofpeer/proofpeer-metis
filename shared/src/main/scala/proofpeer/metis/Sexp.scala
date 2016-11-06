package proofpeer.metis

import org.ensime.sexp._
import LiteralInstances._
import scalaz._
import Scalaz._
import org.ensime.sexp.{ SexpCompactPrinter => SexpPrinter }

/** S-expression syntax for clauses.

  - All variables and function and predicate functors are either symbols are else
    singleton lists ((v)).

  - Terms:

    + function applications are a list whose first element is the functor, and
    whose remaining elements are terms;

  - Atoms:

    + equations are lists of the form (= [p] [q]) where [p] and [q] are terms;

    + predicates are list whose first element is the predicate functor and whose
    remaining elements are terms.

  - Literals

    + negative literals are lists of the form (not p) where p is an atom;

    + positive literals are atoms.
*/
object Sexp {
  type SexpFn = String \/ Sexp
  def functorOfSexp(sexpr: Sexp): SexpFn = sexpr match {
    case SexpSymbol(sym) => -\/(sym)
    case SexpList(List(SexpList(List(sexpr)))) => \/-(sexpr)
  }
  def termOfSexp(sexpr: Sexp): Term[SexpFn, SexpFn] = sexpr match {
    case SexpList(f :: args) => Fun(functorOfSexp(f), args.map(termOfSexp(_)))
    case _ => Var(functorOfSexp(sexpr))
  }
  def atomOfSexp(sexpr: Sexp): Atom[SexpFn, SexpFn, SexpFn] = sexpr match {
    case SexpList(List(SexpSymbol("="), l, r)) =>
      Eql(termOfSexp(l), termOfSexp(r))
    case SexpList(p :: args) =>
      Pred(functorOfSexp(p), args.map(termOfSexp(_)))
  }
  def litOfSexp(sexpr: Sexp): Literal[SexpFn, SexpFn, SexpFn] = sexpr match {
    case SexpList(List(SexpSymbol("not"), p)) =>
      Literal(false,atomOfSexp(p))
    case _ => Literal(true, atomOfSexp(sexpr))
  }
  def clauseOfSexp(sexpr: Sexp): Clause[SexpFn, SexpFn, SexpFn] = sexpr match {
    case SexpList(lits) => Clause(ISet.fromList(lits.map(litOfSexp(_))))
  }

  def SexpOfFn(fn: SexpFn): Sexp = fn match {
    case -\/(fn) => SexpSymbol(fn)
    case \/-(fn) => SexpList(List(SexpList(List(fn))))
  }
  def SexpOfTerm(tm: Term[SexpFn,SexpFn]): Sexp = tm match {
    case Var(v) => SexpOfFn(v)
    case Fun(f,args) => SexpList(SexpOfFn(f) :: args.map(SexpOfTerm(_)))
  }
  def SexpOfAtom(atom: Atom[SexpFn, SexpFn, SexpFn]): Sexp = atom match {
    case Pred(p,args) => SexpList(SexpOfFn(p) :: args.map(SexpOfTerm(_)))
    case Eql(l,r) => SexpList(List(SexpSymbol("="), SexpOfTerm(l), SexpOfTerm(r)))
  }
  def SexpOfLiteral(lit: Literal[SexpFn, SexpFn, SexpFn]): Sexp = lit match {
    case Literal(true, atom) => SexpOfAtom(atom)
    case Literal(false, atom) =>
      SexpList(List(SexpSymbol("not"), SexpOfAtom(atom)))
  }
  def SexpOfClause(cl: Clause[SexpFn, SexpFn, SexpFn]): Sexp =
    SexpList(cl.lits.toList.map(SexpOfLiteral(_)))

  trait FOLSexpPrinter extends Printer[SexpFn,SexpFn,SexpFn] {
    override def printV(v: SexpFn) =
      Cord(SexpPrinter(SexpOfFn(v)))
    override def printF(f: SexpFn) =
      Cord(SexpPrinter(SexpOfFn(f)))
    override def printP(p: SexpFn) =
      Cord(SexpPrinter(SexpOfFn(p)))
    override def printTerm(tm: Term[SexpFn,SexpFn]) =
      Cord(SexpPrinter(SexpOfTerm(tm)))
    override def printAtom(atm: Atom[SexpFn,SexpFn,SexpFn]) =
      Cord(SexpPrinter(SexpOfAtom(atm)))
    override def printLiteral(lit: Literal[SexpFn,SexpFn,SexpFn]) =
      Cord(SexpPrinter(SexpOfLiteral(lit)))
    override def printClause(cl: Clause[SexpFn,SexpFn,SexpFn]) =
      Cord(SexpPrinter(SexpOfClause(cl)))
  }
}
