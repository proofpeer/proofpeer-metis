package proofpeer.metis

import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/** Literals: atomic formulas and their negations.
  * @param isPositive Is this a non-negated atomic formula?
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  */
case class Literal[V:Order,F,P](isPositive: Boolean, atom: Atom[V,F,P])
    extends GenTerm[V,Term[V,F],Literal[V,F,P]] {
  def negate() = Literal(!this.isPositive, this.atom)

  override def frees = atom.frees
  override def freeIn(v: V) = atom.freeIn(v)
  override def subst(θ: Subst[V,Term[V,F]]) = Literal(isPositive,atom.subst(θ))
  override def patMatch(θ: Subst[V,Term[V,F]], lit: Literal[V,F,P]) =
    if (isPositive == lit.isPositive)
      atom.patMatch(θ,lit.atom)
    else List()
  override def unify(θ: Subst[V,Term[V,F]], lit: Literal[V,F,P]) =
    if (isPositive == lit.isPositive)
      atom.unify(θ,lit.atom)
    else List()
  override def heuristicSize = atom.heuristicSize

  def isRefl = isPositive && atom.isRefl
}

class LiteralOrdering[V:Order,F,P](kbo: KnuthBendix[V,F], predicateFunctor: P=>F) {
  def atomToTerms(atm: Atom[V,F,P]) =
    atm match {
      case Pred(p,args) => List(Fun(predicateFunctor(p),args))
      case Eql(l,r)     => List(l,r)
    }

  def strictyLess(tm1: Term[V,F], tm2: Term[V,F]) =
    kbo.tryCompare(tm1,tm2).map { _ == Ordering.LT }.getOrElse(false)

  def notStrictlyLess(tms1: List[Term[V,F]], tms2: List[Term[V,F]]) = {
    !tms1.forall { tm1 =>
      tms2.exists { tm2 =>
        strictyLess(tm1,tm2)
      }
    }
  }

  def isLargerLiteral(lits: Set[Literal[V,F,P]]): Literal[V,F,P] => Boolean = {
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
    def order(lit1: Literal[V,F,P], lit2: Literal[V,F,P]) =
      (lit1.isPositive, lit1.atom) ?|? (lit2.isPositive, lit2.atom)
  }
}
