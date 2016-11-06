package proofpeer.metis.fol

import org.ensime.sexp._
import org.ensime.sexp.Sexp.{ SexpIsOrder }
import scalaz._
import Scalaz._

import proofpeer.metis.Term
import proofpeer.metis.{ Eql, Pred => APred }
import proofpeer.metis.Var
import proofpeer.metis.Sexp._
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
  `proofpeer.metis.Sexp`.

  - Predications of two-arguments whose first element is one of the symbols '!, 'ex,
  'and, 'or, '->' or '<-> must have their predicate symbols disambiguated.

  - Predicates of one-argument whose first element is 'not must have the symbol
  disambiguated.

  Implications and equivalences are sugar.
*/

object Sexp {
  def folOfImplies[B](
    p: FOL[SexpFn, SexpFn, SexpFn, FOL.Neg, B],
    q: FOL[SexpFn, SexpFn, SexpFn, FOL.Neg, B]):
      FOL[SexpFn, SexpFn, SexpFn, FOL.Neg, B] =
    Or(Unary(FOL.Neg(),p),q)

  def folOfSexp(sexpr: Sexp, desugar: Boolean):
      FOL[SexpFn, SexpFn, SexpFn, FOL.Neg, FOL.Binder] =
    sexpr match {
      case SexpList(List(SexpSymbol("!"), v, body)) =>
        Bnding(FOL.All, functorOfSexp(v), folOfSexp(body,desugar))
      case SexpList(List(SexpSymbol("ex"), v, body)) =>
        Bnding(FOL.Exists, functorOfSexp(v), folOfSexp(body,desugar))
      case SexpList(List(SexpSymbol("and"), p, q)) =>
        And(folOfSexp(p,desugar), folOfSexp(q,desugar))
      case SexpList(List(SexpSymbol("or"), p, q)) =>
        Or(folOfSexp(p,desugar),folOfSexp(q,desugar))
      case SexpList(List(SexpSymbol("not"), p)) =>
        Unary(Neg(), folOfSexp(p,desugar))
      case SexpList(List(SexpSymbol("->"), p, q)) if desugar =>
        folOfImplies(folOfSexp(p,desugar), folOfSexp(q,desugar))
      case SexpList(List(SexpSymbol("<->"), p, q)) if desugar =>
        val psexp = folOfSexp(p,desugar)
        val qsexp = folOfSexp(q,desugar)
        And(folOfImplies(psexp, qsexp), folOfImplies(qsexp, psexp))
      case sexp =>
        atomOfSexp(sexpr) match {
          case Eql(x,y) => Pred(functorOfString("="),List(x,y))
          case APred(p,args) => Pred(p,args)
        }
    }
  def SexpOfFol(
    fol: FOL[SexpFn,SexpFn,SexpFn,FOL.Neg,FOL.Binder],
    resugar: Boolean): Sexp =
    fol match {
      case Bnding(FOL.All, v, body) =>
        SexpList(SexpSymbol("!"),SexpOfFn(v),SexpOfFol(body,resugar))
      case Bnding(FOL.Exists, v, body) =>
        SexpList(SexpSymbol("ex"),SexpOfFn(v),SexpOfFol(body,resugar))
      case And(Or(Unary(FOL.Neg(),p1),q1), Or(Unary(FOL.Neg(),q2),p2))
          if resugar && p1 === p2 && q1 === q2 =>
        SexpList(SexpSymbol("<->"), SexpOfFol(p1,resugar), SexpOfFol(q1,resugar))
      case Or(Unary(FOL.Neg(),p),q) if resugar =>
        SexpList(SexpSymbol("->"), SexpOfFol(p,resugar), SexpOfFol(q,resugar))
      case And(p,q) =>
        SexpList(SexpSymbol("and"), SexpOfFol(p,resugar), SexpOfFol(q,resugar))
      case Or(p,q) =>
        SexpList(SexpSymbol("or"), SexpOfFol(p,resugar), SexpOfFol(q,resugar))
      case Unary(FOL.Neg(), p) => SexpList(SexpSymbol("not"), SexpOfFol(p,resugar))
      case Pred(p,args) => SexpList(SexpOfFn(p) :: args.map(SexpOfTerm(_)))
    }
}
