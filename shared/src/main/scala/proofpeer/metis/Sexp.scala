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
object SExpr {
  type SExprFn = String \/ Sexp
  def functorOfSExpr(sexpr: Sexp): SExprFn = sexpr match {
    case SexpSymbol(sym) => -\/(sym)
    case SexpList(List(SexpList(List(sexpr)))) => \/-(sexpr)
  }
  def termOfSExpr(sexpr: Sexp): Term[SExprFn, SExprFn] = sexpr match {
    case SexpList(f :: args) => Fun(functorOfSExpr(f), args.map(termOfSExpr(_)))
    case _ => Var(functorOfSExpr(sexpr))
  }
  def atomOfSExpr(sexpr: Sexp): Atom[SExprFn, SExprFn, SExprFn] = sexpr match {
    case SexpList(List(SexpSymbol("="), l, r)) =>
      Eql(termOfSExpr(l), termOfSExpr(r))
    case SexpList(p :: args) =>
      Pred(functorOfSExpr(p), args.map(termOfSExpr(_)))
  }
  def litOfSExpr(sexpr: Sexp): Literal[SExprFn, SExprFn, SExprFn] = sexpr match {
    case SexpList(List(SexpSymbol("not"), p)) =>
      Literal(false,atomOfSExpr(p))
    case _ => Literal(true, atomOfSExpr(sexpr))
  }
  def clauseOfSExpr(sexpr: Sexp): Clause[SExprFn, SExprFn, SExprFn] = sexpr match {
    case SexpList(lits) => Clause(ISet.fromList(lits.map(litOfSExpr(_))))
  }

  def SExprOfFn(fn: SExprFn): Sexp = fn match {
    case -\/(fn) => SexpSymbol(fn)
    case \/-(fn) => SexpList(List(SexpList(List(fn))))
  }
  def SExprOfTerm(tm: Term[SExprFn,SExprFn]): Sexp = tm match {
    case Var(v) => SExprOfFn(v)
    case Fun(f,args) => SexpList(SExprOfFn(f) :: args.map(SExprOfTerm(_)))
  }
  def SExprOfAtom(atom: Atom[SExprFn, SExprFn, SExprFn]): Sexp = atom match {
    case Pred(p,args) => SexpList(SExprOfFn(p) :: args.map(SExprOfTerm(_)))
    case Eql(l,r) => SexpList(List(SexpSymbol("="), SExprOfTerm(l), SExprOfTerm(r)))
  }
  def SExprOfLiteral(lit: Literal[SExprFn, SExprFn, SExprFn]): Sexp = lit match {
    case Literal(true, atom) => SExprOfAtom(atom)
    case Literal(false, atom) =>
      SexpList(List(SexpSymbol("not"), SExprOfAtom(atom)))
  }
  def SExprOfClause(cl: Clause[SExprFn, SExprFn, SExprFn]): Sexp =
    SexpList(cl.lits.toList.map(SExprOfLiteral(_)))

  trait FOLSExprPrinter extends Printer[SExprFn,SExprFn,SExprFn] {
    override def printV(v: SExprFn) =
      Cord(SexpPrinter(SExprOfFn(v)))
    override def printF(f: SExprFn) =
      Cord(SexpPrinter(SExprOfFn(f)))
    override def printP(p: SExprFn) =
      Cord(SexpPrinter(SExprOfFn(p)))
    override def printTerm(tm: Term[SExprFn,SExprFn]) =
      Cord(SexpPrinter(SExprOfTerm(tm)))
    override def printAtom(atm: Atom[SExprFn,SExprFn,SExprFn]) =
      Cord(SexpPrinter(SExprOfAtom(atm)))
    override def printLiteral(lit: Literal[SExprFn,SExprFn,SExprFn]) =
      Cord(SexpPrinter(SExprOfLiteral(lit)))
    override def printClause(cl: Clause[SExprFn,SExprFn,SExprFn]) =
      Cord(SexpPrinter(SExprOfClause(cl)))
  }
}
