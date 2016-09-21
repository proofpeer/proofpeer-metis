package proofpeer.metis.fol

import org.ensime.sexp._
import scalaz._
import Scalaz._

import proofpeer.metis.{ Term, Var, Fun }

object SExpr {
  def termOfSExpr(sexpr: Sexp): Term[Sexp, Sexp] =
    sexpr match {
      case SexpList(f :: args) => Fun(f, args.map(termOfSExpr(_)))
      case expr => Var(expr)
    }
  def predOfSExpr[U,B](sexpr: Sexp):
      ValidationNel[(Sexp, String), Pred[Sexp, Sexp, Sexp, U, B]] =
    sexpr match {
      case SexpList(p :: args) => Pred(p, args.map(termOfSExpr(_))).success
      case _ => (sexpr, "predicate expected").failureNel
    }
  def folOfImplies[B](
    p: FOL[Sexp, Sexp, Sexp, FOL.Neg, B],
    q: FOL[Sexp, Sexp, Sexp, FOL.Neg, B]):
      FOL[Sexp, Sexp, Sexp, FOL.Neg, B] =
    Or(Unary(FOL.Neg(),p),q)
  def folOfSExpr(sexpr: Sexp):
      ValidationNel[(Sexp, String), FOL[Sexp, Sexp, Sexp, FOL.Neg, FOL.Binder]] =
    sexpr match {
      case SexpList(List(SexpSymbol("∀"), v, body)) =>
        folOfSExpr(body).map { Bnding(FOL.All, v, _) }
      case SexpList(List(SexpSymbol("∃"), v, body)) =>
        folOfSExpr(body).map { Bnding(FOL.Exists, v, _) }
      case SexpList(List(SexpSymbol("∧"), p, q)) =>
        (folOfSExpr(p) |@| folOfSExpr(q)) { And(_,_) }
      case SexpList(List(SexpSymbol("∨"), p, q)) =>
        (folOfSExpr(p) |@| folOfSExpr(q)) { Or(_,_) }
      case SexpList(List(SexpSymbol("¬"), p)) =>
        folOfSExpr(p).map { Unary(FOL.Neg(),_) }
      case SexpList(List(SexpSymbol("→"), p, q)) =>
        (folOfSExpr(p) |@| folOfSExpr(q)) { folOfImplies(_,_) }
      case SexpList(List(SexpSymbol("↔"), p, q)) =>
        (folOfSExpr(p) |@| folOfSExpr(q)) {
          (p,q) => And(
            folOfImplies(p, q),
            folOfImplies(q, p))
        }
      case _ => (sexpr, "formula expected").failureNel
    }
}
