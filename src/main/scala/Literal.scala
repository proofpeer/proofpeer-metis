package proofpeer.metis

import proofpeer.metis.util.{PartialOrder}
import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._

object Literal {
  case class TermCursor[V,F,P] private[Literal] (
    top: Literal[V,F,P],
    cursor: Atom.TermCursor[V,F,P])
      extends GenCursor[V,Term[V,F],Literal[V,F,P],TermCursor[V,F,P]] {

    /** Argument path from theTop to the cursor. */
    def path = cursor.path
    override def get = cursor.get

    override def replaceWith(replacement: Term[V,F]): Literal[V,F,P] =
      new Literal(top.isPositive,cursor.replaceWith(replacement))

    override def substTop(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) = {
      val cursor_ = cursor.substTop(θ)
      val lit_    = Literal(top.isPositive,cursor_.top)
      new TermCursor(lit_,cursor_)
    }
  }
}

/** Literals: atomic formulas and their negations.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  *
  * @param isPositive Is this a non-negated atomic formula?
  */
case class Literal[V,F,P](isPositive: Boolean, atom: Atom[V,F,P])
    extends GenTerm[V,Term[V,F],Literal.TermCursor[V,F,P],Literal[V,F,P]] {
  def negate() = Literal(!this.isPositive, this.atom)

  override def frees = atom.frees
  override def freeIn(v: V) = atom.freeIn(v)
  override def subst(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) =
    Literal(isPositive,atom.subst(θ))
  override def patMatch(θ: Subst[V,Term[V,F]], lit: Literal[V,F,P])(
    implicit ev: Order[V]) =
    if (isPositive == lit.isPositive)
      atom.patMatch(θ,lit.atom)
    else List()
  override def unify(θ: Subst[V,Term[V,F]], lit: Literal[V,F,P])(
    implicit ev: Order[V]) =
    if (isPositive == lit.isPositive)
      atom.unify(θ,lit.atom)
    else List()
  override def heuristicSize = atom.heuristicSize

  override def allSubterms =
    atom.allSubterms.map { new Literal.TermCursor(this,_) }

  /** Is this literal of the form x=x? */
  def isRefl = isPositive && atom.isRefl
}

trait LiteralOrdering[V,F,P] {
  /** isLargerLiteral(lits)(lit) returns true iff lit is maximal in {lit} ∪ lits. */
  def isMaximal(lits: Set[Literal[V,F,P]]): Literal[V,F,P] => Boolean
}

class MetisLiteralOrdering[V:Order,F](termOrd: PartialOrder[Term[V,F]])
    extends LiteralOrdering[V,F,F] {
  def atomToTerms(atm: Atom[V,F,F]) =
    atm match {
      case Pred(p,args) => List(Fun(p,args))
      case Eql(l,r)     => List(l,r)
    }

  def strictyLess(tm1: Term[V,F], tm2: Term[V,F]) =
    termOrd.tryCompare(tm1,tm2).map { _ == Ordering.LT }.getOrElse(false)

  def notStrictlyLess(tms1: List[Term[V,F]], tms2: List[Term[V,F]]) = {
    !tms1.forall { tm1 =>
      tms2.exists { tm2 =>
        strictyLess(tm1,tm2)
      }
    }
  }

  override def isMaximal(lits: Set[Literal[V,F,F]]): Literal[V,F,F] => Boolean = {
    if (lits.isEmpty)
      _ => true
    else {
      val allPositive = lits.forall(_.isPositive)
      val tms = lits.filter { _.isPositive == allPositive }
        .flatMap { lit => atomToTerms(lit.atom) }.toList
      lit => lit match {
        case Literal(isPositive,atom) =>
          if (isPositive == allPositive)
            notStrictlyLess(atomToTerms(atom), tms)
          else
            allPositive
      }
    }
  }
}

object LiteralInstances {
  import AtomInstances._
  implicit def LiteralOrder[V:Order,F:Order,P:Order] = new Order[Literal[V,F,P]] {
    override def order(lit1: Literal[V,F,P], lit2: Literal[V,F,P]) =
      (lit1.isPositive, lit1.atom) ?|? (lit2.isPositive, lit2.atom)
  }

  implicit def Show[V:Show,F:Show,P:Show] = new Show[Literal[V,F,P]] {
    override def show(lit: Literal[V,F,P]) =
      if (!lit.isPositive) Cord("~") else ∅[Cord] ++ lit.atom.show
  }
}
