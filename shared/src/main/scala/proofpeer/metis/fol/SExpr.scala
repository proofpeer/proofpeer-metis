package proofpeer.metis.fol

import org.ensime.sexp._
import org.ensime.sexp.Sexp.{ SexpIsOrder }
import scalaz._
import Scalaz._

import proofpeer.metis.Term
import proofpeer.metis.SExpr._
import FOL._
import FOL.Instances._

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

  Implications and equivalences are sugar.
*/

object SExpr {
  def folOfImplies[B](
    p: FOL[Sexp, Sexp, Sexp, FOL.Neg, B],
    q: FOL[Sexp, Sexp, Sexp, FOL.Neg, B]):
      FOL[Sexp, Sexp, Sexp, FOL.Neg, B] =
    Or(Unary(FOL.Neg(),p),q)

  def folOfSExpr(sexpr: Sexp, desugar: Boolean):
      ValidationNel[(Sexp, String), FOL[Sexp, Sexp, Sexp, FOL.Neg, FOL.Binder]] =
    sexpr match {
      case SexpList(List(SexpList(List(sym@SexpSymbol(str))), x, y))
          if str === "!" || str === "ex" || str === "and" || str === "or" ||
          str == "->" || str == "<->" =>
        Pred(sym:Sexp, List(termOfSExpr(x), termOfSExpr(y))).success
      case SexpList(List(SexpSymbol("!"), v, body)) =>
        folOfSExpr(body,desugar).map { Bnding(FOL.All, v, _) }
      case SexpList(List(SexpSymbol("ex"), v, body)) =>
        folOfSExpr(body,desugar).map { Bnding(FOL.Exists, v, _) }
      case SexpList(List(SexpSymbol("and"), p, q)) =>
        (folOfSExpr(p,desugar) |@| folOfSExpr(q,desugar)) { And(_,_) }
      case SexpList(List(SexpSymbol("or"), p, q)) =>
        (folOfSExpr(p,desugar) |@| folOfSExpr(q,desugar)) { Or(_,_) }
      case SexpList(List(SexpSymbol("not"), p)) =>
        folOfSExpr(p,desugar).map { Unary(FOL.Neg(),_) }
      case SexpList(List(SexpList(List(sym@SexpSymbol("not"))), x)) =>
        Pred(sym:Sexp, List(termOfSExpr(x))).success
      case SexpList(List(SexpSymbol("->"), p, q)) if desugar =>
        (folOfSExpr(p,desugar) |@| folOfSExpr(q,desugar)) { folOfImplies(_,_) }
      case SexpList(List(SexpSymbol("<->"), p, q)) if desugar =>
        (folOfSExpr(p,desugar) |@| folOfSExpr(q,desugar)) {
          (p,q) => And(
            folOfImplies(p, q),
            folOfImplies(q, p))
        }
      case SexpList(p :: args) =>
        Pred(p, args.map(termOfSExpr(_))).success
      case _ => (sexpr, "formula expected").failureNel
    }
  def lambdaOfSExpr(sexpr: Sexp, desugar: Boolean):
      ValidationNel[(Sexp, String),
        Term[Sexp,Sexp] => FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]] =
    sexpr match {
      case SexpList(List(x, body)) =>
        folOfSExpr(body,desugar).map {
          fol => arg:Sexp =>
          trimap(fol)({ y => if (x === y) arg else y }, f => f, p => p)
        }
      case _ => (sexpr, "lambda expected").failureNel
  }
  def SExprOfFol(
    fol: FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder],
    resugar: Boolean): Sexp =
    fol match {
      case Bnding(FOL.All, v, body) =>
        SexpList(SexpSymbol("!"),v,SExprOfFol(body,resugar))
      case Pred(sym@SexpSymbol(str), List(l, r))
          if str === "!" || str === "ex" || str === "and" || str === "or"
          || str == "->" || str == "<->" =>
        SexpList(SexpList(sym),SExprOfTerm(l),SExprOfTerm(r))
      case Bnding(FOL.Exists, v, body) =>
        SexpList(SexpSymbol("ex"),v,SExprOfFol(body,resugar))
      case And(Or(Unary(FOL.Neg(),p1),q1), Or(Unary(FOL.Neg(),q2),p2))
          if resugar && p1 === p2 && q1 === q2 =>
        SexpList(SexpSymbol("<->"), SExprOfFol(p1,resugar), SExprOfFol(q1,resugar))
      case Or(Unary(FOL.Neg(),p),q) if resugar =>
        SexpList(SexpSymbol("->"), SExprOfFol(p,resugar), SExprOfFol(q,resugar))
      case And(p,q) =>
        SexpList(SexpSymbol("and"), SExprOfFol(p,resugar), SExprOfFol(q,resugar))
      case Or(p,q) =>
        SexpList(SexpSymbol("or"), SExprOfFol(p,resugar), SExprOfFol(q,resugar))
      case Unary(FOL.Neg(), p) => SexpList(SexpSymbol("not"), SExprOfFol(p,resugar))
      case Pred(sym@SexpSymbol("not"),List(x)) =>
        SexpList(SexpList(SexpSymbol("not")), SExprOfTerm(x))
      case Pred(p,args) => SexpList(p :: args.map(SExprOfTerm(_)))
    }
  // sexpr should be an s-expression representing a fol without sugar
  def invertTest(sexpr: Sexp) = {
    folOfSExpr(sexpr,false).map { fol =>
      SExprOfFol(fol,false) == sexpr
    }.getOrElse(true)
  }
}
