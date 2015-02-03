package proofpeer.metis

import scala.collection.{GenTraversableOnce}
import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import TermInstances._
import util._
import RichCollectionInstances._

object Clause {
  case class TermCursor[V:Order,F:Order,P:Order](
    top: Clause[V,F,P],
    pos:    Int,
    literalCursor: Literal.TermCursor[V,F,P])
      extends GenCursor[V,Term[V,F],Clause[V,F,P],TermCursor[V,F,P]] {

    override def get = literalCursor.get

    override def replaceWith(subterm: Term[V,F]): Clause[V,F,P] = {
      top.lits.toList.splitAt(pos) match {
        case (pre,arg::sucs) =>
          Clause((pre ++ (literalCursor.replaceWith(subterm)::sucs)).toSet)
        case _ => throw new Error("Bug: No such literal.")
      }
    }

    /** The literal containing the subterm. */
    def literal = top.lits.toList.splitAt(pos) match {
      case (_,arg::_) => arg
      case _          => throw new Error("Bug: No such literal.")
    }

    def substTop(θ: Subst[V,Term[V,F]]) = {
      val cursor_ = literalCursor.substTop(θ)
      val lit_    = cursor_.top
      top.lits.toList.splitAt(pos) match {
        case (pre,_::sucs) =>
          val cl = Clause(
            (pre.map(_.subst(θ)) ++
              (lit_ :: sucs.map(_.subst(θ)))).toSet)
          new TermCursor(cl,pos,cursor_)
        case _ => throw new Error("Bug: No such subterm.")
      }
    }
  }
}

/** Clauses, implicitly a set of literals, of which the clause is their
  * disjunction. Two clauses are equal when their set of literals is equal.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  */
case class Clause[V:Order,F:Order,P:Order](lits: Set[Literal[V,F,P]])
    extends GenTerm[V,Term[V,F],Clause.TermCursor[V,F,P],Clause[V,F,P]] {

  import LiteralInstances._
  import AtomInstances._

  override def frees = lits.flatMap(_.frees)
  override def freeIn(v: V) = lits.exists(_.freeIn(v))
  override def subst(θ: Subst[V,Term[V,F]]) = new Clause(lits.map(_.subst(θ)))
  override def patMatch(θ: Subst[V,Term[V,F]], cl: Clause[V,F,P]) = {
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
  override def unify(θ: Subst[V,Term[V,F]], cl: Clause[V,F,P]) = {
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
  override def heuristicSize = lits.toList.map(_.heuristicSize).sum

  override def allSubterms =
    for (
      (lit,n) <- lits.toList.zipWithIndex;
      cursor  <- lit.allSubterms)
    yield new Clause.TermCursor(this,n,cursor)

  implicit val atomOrder = Order[Atom[V,F,P]].toScalaOrdering

  def isTautology: Boolean = {
    lits.foldLeft(new TreeSet[Atom[V,F,P]]()) {
      case (set,lit) =>
        if (lit.isPositive && lit.isRefl) {
          return true
        }
        else {
          if (set.contains(lit.atom)) {
            return true
          }
          else set + lit.atom
        }
    }
    return false
  }

  def isContradiction = lits.isEmpty

  def largestLiterals(litOrder: LiteralOrdering[V,F,P]) =
    lits.filter(litOrder.isMaximal(this.lits)(_))

  def largestSubterms(litOrder: LiteralOrdering[V,F,P]) = {
    for (
      (lit,n) <- lits.zipWithIndex;
      if litOrder.isMaximal(this.lits)(lit);
      cursor  <- lit.allSubterms)
    yield new Clause.TermCursor(this,n,cursor)
  }
}

object ClauseInstances {
  implicit def toRicIterable[V,F,P](cl: Clause[V,F,P]) =
    new RichIterable(cl.lits)
  implicit def toSet[V,F,P](cl: Clause[V,F,P]) = cl.lits
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
