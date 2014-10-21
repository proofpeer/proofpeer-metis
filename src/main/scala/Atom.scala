package proofpeer.metis

import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import TermInstances._

/** Atomic formulas for first-order logic with equality.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  */
abstract sealed class Atom[V,F,P]
case class Pred[V,F,P](predicate: P, args: List[Term[V,F]]) extends Atom[V,F,P]
case class Eql[V,F,P](l: Term[V,F], r: Term[V,F]) extends Atom[V,F,P] {
  def isRefl() = l == r
}

object AtomInstances {
  implicit def OrdAtom[V,F,P](implicit
    ordV: Order[V],
    ordF: Order[F],
    ordP: Order[P]) = new Order[Atom[V,F,P]] {
      // The definition here is likely to differ from Hurd's. In particular,
      // equalities are assumed to be the smallest of any relation. Hurd, by
      // contrast, doesn't treat equalities as a separate variant and always compares
      // on the relation name.
      def order(atm1: Atom[V,F,P], atm2: Atom[V,F,P]) =
        (atm1,atm2) match {
          case (Pred(p1,args1), Pred(p2,args2)) => (p1,args1) ?|? (p2,args2)
          case (Eql(_,_),Pred(_,_))             => Ordering.LT
          case (Pred(_,_),Eql(_,_))             => Ordering.GT
          case (Eql(l1,r1),Eql(l2,r2))          => (l1,r1) ?|? (l2,r2)
        }
  }
}

object Atom {
  def isRefl[V,F,P](atom: Atom[V,F,P]) =
    atom match {
      case Pred(_,_)      => false
      case eql:Eql[V,F,P] => eql.isRefl
    }

  def subst[V,F,P](theta: Term.Subst[V,F], atm: Atom[V,F,P]): Atom[V,F,P] =
    // Removed optimisation from SML: we don't bother with a pointer equality check
    atm match {
      case Pred(p,args) => Pred(p,args.map(Term.subst(theta,_)))
      case Eql(l,r)     => Eql(Term.subst(theta,l),Term.subst(theta,r))
  }
}
