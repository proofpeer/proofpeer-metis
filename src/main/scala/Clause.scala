package proofpeer.metis

import AtomInstances._
import scala.collection.{GenTraversableOnce}
import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import TermInstances._
import util._
import RichCollectionInstances._

/** Clauses, implicitly a set of literals, of which the clause is their
    disjunction. Two clauses are equal when their set of literals is equal. */
case class Clause[V,F,P](lits: Set[Literal[V,F,P]])(implicit
  ordV: Order[V],
  ordF: Order[F],
  ordP: Order[P]) extends GenTerm[V,Term[V,F],Clause[V,F,P]] {

  import LiteralInstances._
  implicit val OrdLiteral = LiteralOrder(ordV,ordF,ordP).toScalaOrdering

  def frees = lits.flatMap(_.frees)
  def freeIn(v: V) = lits.exists(_.freeIn(v))
  def subst(θ: Subst[V,Term[V,F]]) = new Clause(lits.map(_.subst(θ)))
  def patMatch(θ: Subst[V,Term[V,F]], cl: Clause[V,F,P]) = {
    def patMatchOrdered(θ: Subst[V,Term[V,F]], ordLits: Seq[Literal[V,F,P]]) = {
      (lits zip ordLits).foldLeftM(θ) {
        case (θ, (lit1,lit2)) => lit1.patMatch(θ,lit2)
      }
    }
    (for (
      perm <- cl.lits.toSeq.permutations;
      θ    <- patMatchOrdered(θ, Seq() ++ perm))
    yield θ).toList
  }
  def unify(θ: Subst[V,Term[V,F]], cl: Clause[V,F,P]) = {
    def unifyMatchOrdered(θ: Subst[V,Term[V,F]], ordLits: Seq[Literal[V,F,P]]) = {
      (lits zip ordLits).foldLeftM(θ) {
        case (θ, (lit1,lit2)) => lit1.patMatch(θ,lit2)
      }
    }
    (for (
      perm <- cl.lits.toSeq.permutations;
      θ    <- unifyMatchOrdered(θ, Seq() ++ perm))
    yield θ).toList
  }

  def isTautology: Boolean = {
    lits.foldLeft(new TreeSet[Literal[V,F,P]]()) {
      case (set,lit) =>
        if (lit.isPositive && lit.isRefl) {
          return true
        }
        else {
          if (set.contains(lit)) {
            return true
          }
          else set + lit
        }
    }
    return false
  }

  def isContradiction = lits.isEmpty

  def largestLiterals(litOrder: LiteralOrdering[V,F,P]) = {
    lits.filter(litOrder.isLargerLiteral(this.lits)(_))
  }

  /** Is this a clause of exactly one literal? (a trivial disjunction) */
  def isUnit =
    this match {
      case UnitClause(_) => true
      case _             => false
    }

  /** Is this a clause of exactly one equation? */
  def isUnitEql =
    this match {
      case UnitClause(Literal(p,Eql(_,_))) => p
      case _                               => false
    }
}

object ClauseInstances {
  implicit def toRichCollection[V,F,P](cl: Clause[V,F,P]) =
    new RichCollection(cl.lits)
  implicit def toSet[V,F,P](cl: Clause[V,F,P]) = cl.lits
}


/** Destruct a clause of exactly only literal. */
object UnitClause {
  def unapply[V,F,P](cl: Clause[V,F,P]): Option[Literal[V,F,P]] = {
    val (unit,rest) = cl.lits.toIterable.splitAt(1)
    unit.toList match {
      case List()                  => None
      case unit::_ if rest.isEmpty => Some(unit)
      case _                       => None
    }
  }
}

/** Destruct a clause of the form ¬(x=x) */
object IrreflLit {
  def unapply[V,F,P](lit: Literal[V,F,P]): Option[Term[V,F]] =
    (lit.isPositive,lit.atom) match {
      case (false,Eql(l,r)) if l == r => Some(l)
      case _                          => None
    }
}

/** Destruct a clause of the form ¬(x=y) */
object NeqLit {
  def unapply[V,F,P](lit: Literal[V,F,P]): Option[(Term[V,F],Term[V,F])] =
    (lit.isPositive,lit.atom) match {
      case (false,Eql(l,r)) => Some(l,r)
      case _                => None
    }
}
