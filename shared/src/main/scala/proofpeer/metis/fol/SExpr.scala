package proofpeer.metis.fol

import org.ensime.sexp._
import scalaz._
import Scalaz._

import proofpeer.metis.SExpr._

/** S-expression syntax for FOL

  - Universally and existentially quantified formulas are respectively represented by
  the lists (! v body) and (ex v body) where v is an arbitrary s-expression
  representing the variable and body is a first-order formula.

  - Conjunctions, disjunctions, implications and equivalences are respectively
  represented by the lists (and p q), (or p q), (-> p q) and (<-> p q) where p and q
  are first-order formulas.

  - Negations are representated by the list (not p) where p is a first-order formula.

  - All other lists are predications whose first-element is the predicate symbol and
  whose remaining elements are terms, as defined in `proofpeer.metis.SExpr`.

  - Predications of two-arguments whose first element is one of the symbols '!, 'ex,
  'and, 'or, '->' or '<-> must have their predicate symbols disambiguated.

  - Predicates of one-argument whose first element is 'not must have the symbol
  disambiguated.
*/

object SExpr {
  def folOfImplies[B](
    p: FOL[Sexp, Sexp, Sexp, FOL.Neg, B],
    q: FOL[Sexp, Sexp, Sexp, FOL.Neg, B]):
      FOL[Sexp, Sexp, Sexp, FOL.Neg, B] =
    Or(Unary(FOL.Neg(),p),q)
  def folOfSExpr(sexpr: Sexp):
      ValidationNel[(Sexp, String), FOL[Sexp, Sexp, Sexp, FOL.Neg, FOL.Binder]] =
    sexpr match {
      case SexpList(List(SexpList(List(sym@SexpSymbol(str))), x, y))
          if str === "!" || str === "?" || str === "and" || str === "or" ||
          str == "->" || str == "<->" =>
        Pred(sym:Sexp, List(termOfSExpr(x), termOfSExpr(y))).success
      case SexpList(List(SexpSymbol("!"), v, body)) =>
        folOfSExpr(body).map { Bnding(FOL.All, v, _) }
      case SexpList(List(SexpSymbol("?"), v, body)) =>
        folOfSExpr(body).map { Bnding(FOL.Exists, v, _) }
      case SexpList(List(SexpSymbol("and"), p, q)) =>
        (folOfSExpr(p) |@| folOfSExpr(q)) { And(_,_) }
      case SexpList(List(SexpSymbol("or"), p, q)) =>
        (folOfSExpr(p) |@| folOfSExpr(q)) { Or(_,_) }
      case SexpList(List(SexpSymbol("not"), p)) =>
        folOfSExpr(p).map { Unary(FOL.Neg(),_) }
      case SexpList(List(SexpList(List(sym@SexpSymbol("not"))), x)) =>
        Pred(sym:Sexp, List(termOfSExpr(x))).success
      case SexpList(List(SexpSymbol("->"), p, q)) =>
        (folOfSExpr(p) |@| folOfSExpr(q)) { folOfImplies(_,_) }
      case SexpList(List(SexpSymbol("<->"), p, q)) =>
        (folOfSExpr(p) |@| folOfSExpr(q)) {
          (p,q) => And(
            folOfImplies(p, q),
            folOfImplies(q, p))
        }
      case SexpList(p :: args) =>
        Pred(p, args.map(termOfSExpr(_))).success
      case _ => (sexpr, "formula expected").failureNel
    }
  def SExprOfFol(fol: FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]): Sexp =
    fol match {
      case Bnding(FOL.All, v, body) =>
        SexpList(SexpSymbol("!"),v,SExprOfFol(body))
      case Pred(sym@SexpSymbol(str), List(l, r))
          if str === "!" || str === "?" || str === "and" || str === "or"
          | str == "->" || str == "<->" =>
        SexpList(SexpList(sym),SExprOfTerm(l),SExprOfTerm(r))
      case Bnding(FOL.Exists, v, body) =>
        SexpList(SexpSymbol("ex"),v,SExprOfFol(body))
      case And(p,q) =>
        SexpList(SexpSymbol("and"), SExprOfFol(p), SExprOfFol(q))
      case Or(p,q) => SexpList(SexpSymbol("or"), SExprOfFol(p), SExprOfFol(q))
      case Unary(FOL.Neg(), p) => SexpList(SexpSymbol("not"), SExprOfFol(p))
      case Pred(sym@SexpSymbol("not"),List(x)) =>
        SexpList(SexpList(SexpSymbol("not")), SExprOfTerm(x))
      case Pred(p,args) => SexpList(p :: args.map(SExprOfTerm(_)))
    }
  def invert(str: String) = {
    folOfSExpr(SexpParser.parse(str)).map { sexpr =>
      SexpCompactPrinter(SExprOfFol(sexpr))
    }
  }
}
