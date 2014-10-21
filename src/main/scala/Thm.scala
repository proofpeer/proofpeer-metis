package proofpeer.metis

import AtomInstances._
import scala.collection.{GenTraversableOnce}
import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import TermInstances._
import util.RichCollectionInstances._

/** Literals: atomic formulas and their negations.
  * @param polarity Is this a non-negated atomic formula?
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  */
case class Literal[V,F,P](polarity: Boolean, atom: Atom[V,F,P]) {
  def subst(theta: Term.Subst[V,F]) =
    Literal[V,F,P](this.polarity, Atom.subst(theta,this.atom))

  def negate() =
    Literal[V,F,P](!this.polarity, this.atom)
}

object LiteralInstances {
  implicit def LiteralOrder[V,F,P](implicit
    ordV: Order[V],
    ordF: Order[F],
    ordP: Order[P]) = new Order[Literal[V,F,P]] {
    def order(lit1: Literal[V,F,P], lit2: Literal[V,F,P]) =
      (lit1.polarity, lit1.atom) ?|? (lit2.polarity, lit2.atom)
  }
}

object Inference extends Enumeration {
  type Inference = Value
  val
    Axiom,
    Assume,
    Refl, Sym, Trans,
    RemoveSym,
    Subst, Factor, Resolve,
    Equality, Irreflexive = Inference
}

class Thm[V,F,P] private (
  theClause: Set[Literal[V,F,P]],
  rule:      Inference.type,
  preds:     List[Thm[V,F,P]])(implicit
    ordV: Order[V],
    ordF: Order[F],
    ordP: Order[P]) {

  import LiteralInstances._
  implicit val OrdLiteral = LiteralOrder(ordV,ordF,ordP).toScalaOrdering
  type Clause = Set[Literal[V,F,P]]

  val clause = theClause

  def isTautology: Boolean = {
    clause.foldLeft(new TreeSet[Literal[V,F,P]]()) {
      case (set,lit) =>
        if (lit.polarity && Atom.isRefl(lit.atom)) {
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

  def isContradiction = clause.isEmpty
}

object Thm {
  /** A logical kernel for resolution proving according based on total orders of the
    * three alphabets. */
  class Kernel[V,F,P](implicit
    ordV: Order[V],
    ordF: Order[F],
    ordP: Order[P]) {

    import LiteralInstances._
    implicit val OrdLiteral = LiteralOrder(ordV,ordF,ordP).toScalaOrdering
    type Clause = Set[Literal[V,F,P]]

    private def mkThm(
      lits: GenTraversableOnce[Literal[V,F,P]],
      rule: Inference.type,
      preds: List[Thm[V,F,P]]) =
      new Thm(
        new TreeSet[Literal[V,F,P]] ++ lits,
        rule,
        preds)

    /**
      *  ------------------- axiom C
      *            C
      */
    def axiom(cl: Set[Literal[V,F,P]]): Thm[V,F,P] =
      new Thm[V,F,P](cl,Inference.Axiom,List())

    /**
      *  ------------------- assume p
      *        p ∨ ¬p
      */
    def assume(lit: Literal[V,F,P]) =
      mkThm(List(lit,lit.negate),Inference.Assume,List())

    /**
      *  ------------------- refl x
      *         x = x
      */
    def refl(tm: Term[V,F]) =
      mkThm(List(Literal(true,Eql(tm,tm))),Inference.Refl,List())

    /** L ∨ C      ¬L ∨ D
      *  ------------------ resolve L
      *       C ∨ D
      */
    def resolve(lit: Literal[V,F,P], thm1: Thm[V,F,P], thm2: Thm[V,F,P]):
        Option[Thm[V,F,P]] = {
      val negLit = lit.negate
      // Could push check onto caller for possible optimisation
      if (thm1.clause.contains(lit) && thm2.clause.contains(negLit)) {
        Some(mkThm(
          (thm1.clause - lit) ++ (thm2.clause - negLit),
          Inference.Resolve,
          List(thm1,thm2)))
      }
      else None
    }

    /**     C
      *  ------- subst θ
      *    C[θ]
      */
    def subst(theta: Term.Subst[V,F], thm: Thm[V,F,P]): Thm[V,F,P] =
      // For now, removing optimisation where, if the entire clause is unchanged, we
      // do not return a newly constructed clause.
      new Thm(thm.clause.map(_.subst(theta)),Inference.Subst,List(thm))

    // Derived rules in Hurd. Primitive here.
    // ======================================

    /** ---------------------- sym x y
      *    ¬(x = y) ∨ (y = x)
      */
    def sym(x: Term[V,F], y: Term[V,F]) =
      mkThm(
        List(Literal(false,Eql(x,y)),Literal(true,Eql(y,x))),
        Inference.Sym,
        List())

    /**      C ∨ ~(x = x)
      *  ------------------- removeIrrefl
      *           C
      */
    def removeIrrefl(thm: Thm[V,F,P]): Thm[V,F,P] =
      new Thm(
        thm.clause.filter {
          case IrreflLit(_) => false
          case _            => true
        },
        Inference.Irreflexive,
        List(thm))

    /**  (x = y) ∨ (y = x) ∨ P
      *  ----------------------- removeSym
      *            C
      */
    def removeSym(thm: Thm[V,F,P]): Thm[V,F,P] =
      new Thm(
        thm.clause.distinctBy {
          case (Literal(p1,Eql(x1,y1)),Literal(p2,Eql(x2,y2))) =>
            x1 == y2 && x2 == y1 && p1 == p2
          case _ => false
        },
        Inference.RemoveSym,
        List(thm))

    def isUnit(thm: Thm[V,F,P]) =
      thm match {
        case UnitThm(_) => true
        case _          => false
      }

    def isUnitRefl(thm: Thm[V,F,P]) =
      thm match {
        case UnitThm(Literal(_,Eql(_,_))) => true
        case _                            => false
      }
  }
}

/** Destruct a theorem of exactly only literal. */
object UnitThm {
  def unapply[V,F,P](thm: Thm[V,F,P]): Option[Literal[V,F,P]] = {
    val (unit,rest) = thm.clause.toIterable.splitAt(1)
    unit match {
      case List()                  => None
      case unit::_ if rest.isEmpty => Some(unit)
      case _                       => None
    }
  }
}

/** Destruct a literal of the form ¬(x=x) */
object IrreflLit {
  def unapply[V,F,P](lit: Literal[V,F,P]): Option[Term[V,F]] =
    (lit.polarity,lit.atom) match {
      case (false,Eql(l,r)) if l == r => Some(l)
      case _                          => None
    }
}

/** Destruct a literal of the form ¬(x=y) */
object NeqLit {
  def unapply[V,F,P](lit: Literal[V,F,P]): Option[(Term[V,F],Term[V,F])] =
    (lit.polarity,lit.atom) match {
      case (false,Eql(l,r)) => Some(l,r)
      case _                => None
    }
}

/** Derived rules */
object Rule {
  /** A set of derived rules based on orderings of the three alphabets.  */
  def derived[V,F,P](implicit
    ordV: Order[V],
    ordF: Order[F],
    ordP: Order[P]) = new {
    val k = new Thm.Kernel[V,F,P]

    // No support for typed vars as yet (do we care about them for pp?)
    /** ~(v = t) ∨ C
      *  ------------ expandAbbrevs
      *     C[t/v]
      */
    def expandAbbrevs(thm: Thm[V,F,P]) = {
      val firstSubst =
        thm.clause.toIterator.map {
          case NeqLit(l,r) if l != r => Term.termUnify(PartialFunction.empty,l,r)
          case _                     => None
        }.find (_.isDefined).flatten
      firstSubst.map(k.subst(_: Term.Subst[V,F],thm)).getOrElse(k.removeIrrefl(thm))
    }

    /** Simplify: chuck out if its a tautology. Otherwise expand all abbreviations
      * until a fixpoint is reached.
      *
      */
    def simplify(thm: Thm[V,F,P]): Option[Thm[V,F,P]] = {
      if (thm.isTautology) {
        None
      }
      else {
        if (thm == k.removeSym(expandAbbrevs(thm))) {
          Some(thm)
        }
        else simplify(thm)
      }
    }
  }
}
