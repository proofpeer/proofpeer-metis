package proofpeer.metis.fol

import org.ensime.sexp._
import org.ensime.sexp.Sexp.{ SexpIsOrder }
import scalaz._
import Scalaz._

import proofpeer.metis.Term
import proofpeer.metis.{ Eql, Pred => APred }
import proofpeer.metis.Var
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

  - All other lists are predications, with syntax identical to atoms as defined in
  `proofpeer.metis.SExpr`.

  - Predications of two-arguments whose first element is one of the symbols '!, 'ex,
  'and, 'or, '->' or '<-> must have their predicate symbols disambiguated.

  - Predicates of one-argument whose first element is 'not must have the symbol
  disambiguated.

  Implications and equivalences are sugar.
*/

object SExpr {
  def folOfImplies[B](
    p: FOL[SExprFn, SExprFn, SExprFn, FOL.Neg, B],
    q: FOL[SExprFn, SExprFn, SExprFn, FOL.Neg, B]):
      FOL[SExprFn, SExprFn, SExprFn, FOL.Neg, B] =
    Or(Unary(FOL.Neg(),p),q)

  def folOfSExpr(sexpr: Sexp, desugar: Boolean):
      FOL[SExprFn, SExprFn, SExprFn, FOL.Neg, FOL.Binder] =
    sexpr match {
      case SexpList(List(SexpSymbol("!"), v, body)) =>
        Bnding(FOL.All, functorOfSExpr(v), folOfSExpr(body,desugar))
      case SexpList(List(SexpSymbol("ex"), v, body)) =>
        Bnding(FOL.Exists, functorOfSExpr(v), folOfSExpr(body,desugar))
      case SexpList(List(SexpSymbol("and"), p, q)) =>
        And(folOfSExpr(p,desugar), folOfSExpr(q,desugar))
      case SexpList(List(SexpSymbol("or"), p, q)) =>
        Or(folOfSExpr(p,desugar),folOfSExpr(q,desugar))
      case SexpList(List(SexpSymbol("not"), p)) =>
        Unary(Neg(), folOfSExpr(p,desugar))
      case SexpList(List(SexpSymbol("->"), p, q)) if desugar =>
        folOfImplies(folOfSExpr(p,desugar), folOfSExpr(q,desugar))
      case SexpList(List(SexpSymbol("<->"), p, q)) if desugar =>
        val psexp = folOfSExpr(p,desugar)
        val qsexp = folOfSExpr(q,desugar)
        And(folOfImplies(psexp, qsexp), folOfImplies(qsexp, psexp))
      case sexp =>
        atomOfSExpr(sexpr) match {
          case Eql(x,y) => Pred(-\/("="),List(x,y))
          case APred(p,args) => Pred(p,args)
        }
    }
  def SExprOfFol(
    fol: FOL[SExprFn,SExprFn,SExprFn,FOL.Neg,FOL.Binder],
    resugar: Boolean): Sexp =
    fol match {
      case Bnding(FOL.All, v, body) =>
        SexpList(SexpSymbol("!"),SExprOfFn(v),SExprOfFol(body,resugar))
      case Bnding(FOL.Exists, v, body) =>
        SexpList(SexpSymbol("ex"),SExprOfFn(v),SExprOfFol(body,resugar))
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
      case Pred(p,args) => SexpList(SExprOfFn(p) :: args.map(SExprOfTerm(_)))
    }
}
