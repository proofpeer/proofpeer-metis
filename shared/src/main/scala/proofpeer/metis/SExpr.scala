package proofpeer.metis

import org.ensime.sexp._
import LiteralInstances._
import scalaz._
import Scalaz._

/** S-expression syntax for clauses.

  - Terms:

    + function applications are a list whose first element is the functor, and
    whose remaining elements are terms;

    + variables are any other s-expression.

  - Atoms:

    + equations are lists of the form (= [p] [q]) where [p] and [q] are terms;

    + predications of 1 or more arguments are lists whose first element is the
    predicate and whose remaining elements are terms;

    + predications of two-arguments whose predicate symbol is '= must have the
    predicate symbol disambiguated;

    + predications of one-argument whose predicate symbol is 'not and which appear
    in positive literal position must have their predicate symbol disambiguated;

    + anything else is a propositional constant.

  - Literals

    + negative literals are lists of the form (not p) where p is an atom;

    + positive literals are atoms.

  A symbol is disambiguated by enclosing it in a list.
*/
object SExpr {
  def termOfSExpr(sexpr: Sexp): Term[Sexp, Sexp] = sexpr match {
    case SexpList(SexpList(List(f)) :: args) => Fun(f, args.map(termOfSExpr(_)))
    case expr => Var(expr)
  }
  def atomOfSExpr(sexpr: Sexp): Atom[Sexp, Sexp, Sexp] = sexpr match {
    case SexpList(List(SexpSymbol("="), l, r)) =>
      Eql(termOfSExpr(l), termOfSExpr(r))
    case SexpList(List(SexpList(List(sym@SexpSymbol("="))), l, r)) =>
      Pred(sym:Sexp, List(termOfSExpr(l), termOfSExpr(r)))
    case SexpList(List(SexpSymbol("="), l, r)) =>
      Eql(termOfSExpr(l), termOfSExpr(r))
    case SexpList(p :: arg :: args) =>
      Pred(p, termOfSExpr(arg) :: args.map(termOfSExpr(_)))
    case p => Pred(p, List())
  }
  def litOfSExpr(sexpr: Sexp): Literal[Sexp, Sexp, Sexp] = sexpr match {
    case SexpList(List(SexpSymbol("not"), p)) =>
      Literal(false,atomOfSExpr(p))
    case SexpList(List(SexpList(List(sym@SexpSymbol("not"))), p)) =>
      Literal(true, Pred(sym, List(termOfSExpr(p))))
    case _ => Literal(true, atomOfSExpr(sexpr))
  }
  def clauseOfSExpr(sexpr: Sexp):
      ValidationNel[(Sexp, String), Clause[Sexp, Sexp, Sexp]] = sexpr match {
    case SexpList(lits) =>
      Clause(ISet.fromList(lits.map(litOfSExpr(_)))).success
    case _ => (sexpr, "clause expected").failureNel
  }
  def SExprOfTerm(tm: Term[Sexp,Sexp]): Sexp = tm match {
    case Var(v) => v
    case Fun(f, args) => SexpList(SexpList(f) :: args.map(SExprOfTerm(_)))
  }
  def SExprOfAtom(pred: Atom[Sexp, Sexp, Sexp]): Sexp = pred match {
    case Pred(sym@SexpSymbol("not"), List(p)) =>
      SexpList(List(SexpList(sym), SExprOfTerm(p)))
    case p => SExprOfNegAtom(p)
  }
  def SExprOfNegAtom(pred: Atom[Sexp, Sexp, Sexp]): Sexp = pred match {
    case Eql(l,r) => SexpList(SexpSymbol("="), SExprOfTerm(l), SExprOfTerm(r))
    case Pred(sym@SexpSymbol("="), List(x,y)) =>
      SexpList(List(SexpList(sym), SExprOfTerm(x), SExprOfTerm(y)))
    case Pred(p, List()) => p
    case Pred(p, args) =>
      SexpList(p :: args.map(SExprOfTerm(_)))
  }
  def SExprOfLit(lit: Literal[Sexp, Sexp, Sexp]): Sexp = lit match {
    case Literal(true, atom) => SExprOfAtom(atom)
    case Literal(false, atom) =>
      SexpList(List(SexpSymbol("not"), SExprOfNegAtom(atom)))
  }
  def SExprOfClause(cl: Clause[Sexp, Sexp, Sexp]): Sexp =
    SexpList(cl.lits.toList.map(SExprOfLit(_)))
}
