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
abstract sealed class Atom[V,F,P](implicit ordV : Order[V])
    extends GenTerm[V,Term[V,F],Atom[V,F,P]] {

  override def frees = this match {
    case Pred(_,args) => args.foldLeft(Set[V]()){
      case (fvs,arg) => fvs union arg.frees
    }
    case Eql(l,r) => l.frees union r.frees
  }

  override def freeIn(v: V) = this match {
    case Pred(_,args) => args.exists(_.freeIn(v))
    case Eql(l,r)     => l.freeIn(v) || r.freeIn(v)
  }

  override def subst(θ: Subst[V,Term[V,F]]): Atom[V,F,P] =
    this match {
      case Pred(p,args) => Pred(p,args.map(_.subst(θ)))
      case Eql(l,r)     => Eql(l.subst(θ),r.subst(θ))
  }

  override def patMatch(θ: Subst[V,Term[V,F]], atm: Atom[V,F,P]):
      List[Subst[V,Term[V,F]]] =
    (this,atm) match {
      case (Pred(p1,args1), Pred(p2,args2))
          if p1 == p2 && args1.length == args2.length =>
        (args1 zip args2).foldLeftM(θ) {
          case (θ, (arg1,arg2)) => arg1.patMatch(θ, arg2)
        }
      case (Eql(l1,r1), Eql(l2,r2)) =>
        for (
          θ <- l1.patMatch(θ,l2);
          θ <- r1.patMatch(θ,r2))
        yield θ
      case _ => List()
    }

  override def unify(θ: Subst[V,Term[V,F]], atm: Atom[V,F,P]):
      List[Subst[V,Term[V,F]]] =
    (this,atm) match {
      case (Pred(p1,args1), Pred(p2,args2))
          if p1 == p2 && args1.length == args2.length =>
        (args1 zip args2).foldLeftM(θ) {
          case (θ, (arg1,arg2)) => arg1.unify(θ, arg2)
        }
      case (Eql(l1,r1), Eql(l2,r2)) =>
        for (
          θ <- l1.unify(θ,l2);
          θ <- r1.unify(θ,r2))
        yield θ
      case _ => List()
    }

  override def heuristicSize = 1 + (this match {
    case Pred(_,args) => args.map(_.heuristicSize).sum
    case Eql(x,y)     => x.heuristicSize + y.heuristicSize
  })

  def isRefl = this match {
    case Eql(l,r) => l == r
    case _        => false
  }
}

case class Pred[V,F,P](functor: P, args: List[Term[V,F]])(
  implicit ordV: Order[V]) extends Atom[V,F,P]
case class Eql[V,F,P](l: Term[V,F], r: Term[V,F])(
  implicit ordV: Order[V]) extends Atom[V,F,P] {
}

object AtomInstances {
  implicit def AtomOrder[V,F,P](implicit
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
