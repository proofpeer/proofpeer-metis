package proofpeer.metis

import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/** Terms as in first-order logic; i.e. the structured elements that are arguments to
  * predicates and that appear on the left- and right-hand sides of equations.
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  */
abstract sealed class Term[V,F]
case class Var[V,F](v: V) extends Term[V,F]
case class Fun[V,F](f: F, args: List[Term[V,F]]) extends Term[V,F]

object TermInstances {
  implicit def OrdTerm[V,F](implicit
    ordV: Order[V],
    ordF: Order[F]) : Order[Term[V,F]] =
    new Order[Term[V,F]]{
      def order (x:Term[V,F], y:Term[V,F]): Ordering =
        (x,y) match {
          case (Var(v1),Var(v2)) => ordV(v1,v2)
          case (Var(_), Fun(_,_))  => Ordering.LT
          case (Fun(_,_), Var(_)) => Ordering.GT
          case (Fun(f1,args1),Fun(f2,args2)) =>
            (args1.length,f1,args1) ?|? (args2.length,f2,args2)
        }
    }
}

object Term {
  type Subst[V,F] = PartialFunction[V,Term[V,F]]
  private def mapping[V,F](x:V, y:Term[V,F])(implicit ord: Order[V]) = {
    implicit val ordV = ord.toScalaOrdering
    new TreeMap() + (x → y)
  }

  // Removed optimisations from the SML:
  //   * In Hurd's, a substitution is a map and can be checked for emptiness, in
  //     which case the term is not traversed. We could go with this, but would have
  //     to use a more specific type than PartialFunction to represent theta.
  //   * As in much of the HOL Light kernel code, terms are not reconstructed if
  //     the constructor arguments are pointer-equal. Could go for this without
  //     having to change any types.
  def subst[V,F](theta: Subst[V,F],term: Term[V,F]) = {
    def sub(term: Term[V,F]): Term[V,F] = {
      term match {
        case Var(v)      => theta.lift(v).getOrElse(term)
        case Fun(f,args) => Fun(f,args.map(sub))
      }
    }
    sub(term)
  }

  def termMatch[V,F](
    theta: Subst[V,F],
    term1: Term[V,F],
    term2: Term[V,F])(implicit ordV: Order[V]): Option[Subst[V,F]] = {
    (term1,term2) match {
      case (Var(v),tm) =>
        theta.lift(v) match {
          case None => Some(theta orElse (mapping(v,tm)))
          case img  => for ( theTm <- img if tm == theTm ) yield theta
        }
      case (Fun(f1,args1), Fun(f2,args2))
          if f1 == f2 && args1.length == args2.length =>
        (args1 zip args2).foldLeftM(theta) {
          (theta:Subst[V,F], args:(Term[V,F],Term[V,F])) => {
            args match {
              case (arg1,arg2) => termMatch(theta,arg1,arg2)
            }
          }
        }
      case _ => None
    }
  }

  def freeIn[V,F](
    v: V,
    term: Term[V,F]): Boolean = {
    term match {
      case Var(v_) => v == v_
      case Fun(_,args) => args.exists(freeIn(v,_))
    }
  }

  def termUnify[V,F](
    theta: Subst[V,F],
    term1: Term[V,F],
    term2: Term[V,F])(implicit ordV: Order[V]): Option[Subst[V,F]] = {
    (term1,term2) match {
      case (Var(v1),Var(v2)) if v1 == v2 => Some(theta)
      case (Var(v),_) if freeIn (v,term2) => None
      case (Var(v),_) =>
        theta.lift(v) match {
          case None =>
            Some (mapping(v,term2) andThen { term => subst(theta,term) })
          case Some(term) => termUnify(theta,term,term2)
        }
      case (_,Var(v)) => termUnify(theta,term2,term1)
      case (Fun(f1,args1),(Fun(f2,args2)))
          if f1 == f2 && args1.length == args2.length =>
        (args1 zip args2).foldLeftM(theta) {
          (theta, args) => {
            args match {
              case (arg1,arg2) => termMatch(theta,arg1,arg2)
            }
          }
        }
    }
  }
}

case class Weight[V] private (nameMap : Map[V,Int], c: Int) {
  import Scalaz._

  def +(w: Weight[V]) =
    Weight(
      nameMap.unionWith(w.nameMap)((_: Int) + (_: Int)),
      c + w.c)

  def unary_- = Weight(nameMap ∘ (-_),-c)

  def -(w: Weight[V]) = this + -w

  def isZero = nameMap.isEmpty && c == 0

  def lowerBound =
    if (nameMap.exists { case (_,n) => n < 0 })
      None
    else Some(c)
}

object Weight {
  import Scalaz._

  def fromTerm[V,F](
    tm: Term[V,F],
    funWeight: Fun[V,F] => Int)(implicit
    ordV: Order[V]) = {
    def wt(w: Weight[V],tms: List[Term[V,F]]): Weight[V] =
      tms match {
        case List() => w
        case Var(name)::tms =>
          wt(
            Weight(
              w.nameMap.insertWith(name,1) {
                case (_,n) => n+1
              },w.c+1),
            tms)
        case (fun@Fun(f,args))::tms =>
          wt(Weight(
            w.nameMap,
            w.c + funWeight(fun)),args ++ tms)
      }
    implicit val ord = ordV.toScalaOrdering
    wt(Weight(new TreeMap[V,Int],-1),List(tm))
  }
}

/** The Knuth Bendix term order.
  *
  * @param funWeight An arbitrary weighting of functor names. Defaults to a constant
  * function in METIS.
  */
class KnuthBendix[V,F](funWeight: Fun[V,F] => Int)(implicit
  ordInt: Order[Int], ordV: Order[V], ordF: Order[F], ordFun: Order[Fun[V,F]]) {

  /** Add weight to a term, so that it can be compared. */
  private case class WTerm(tm:Term[V,F]) {
    val weight = Weight.fromTerm(tm,funWeight)
    def weightOf(tm: Term[V,F]) = Weight.fromTerm(tm,funWeight)
    def kbOrder(otherTm: WTerm) = {
      def weightCmp(tm1: Term[V,F], tm2: Term[V,F]): Option[Ordering] = {
        val w = weightOf(tm2) - weightOf(tm1)
        if (w.isZero)
          precedenceCmp(tm1,tm2)
        else if (weightDiffLess(w,tm1,tm2))
          Some(Ordering.LT)
        else if (weightDiffGreater(w,tm1,tm2))
          Some(Ordering.GT)
        else
          None
      }
      def weightLess(tm1: Term[V,F], tm2: Term[V,F]): Boolean = {
        val w = weightOf(tm2) - weightOf(tm1)
        if (w.isZero)
          precedenceLess(tm1,tm2)
        else weightDiffLess(w,tm1,tm2)
      }
      def weightDiffLess(w: Weight[V], tm1: Term[V,F], tm2: Term[V,F]): Boolean =
        w.lowerBound match {
          case None    => false
          case Some(0) => precedenceLess(tm1,tm2)
          case Some(n) => n > 0
        }
      def weightDiffGreater(w: Weight[V], tm1: Term[V,F], tm2: Term[V,F]) =
        weightDiffLess (-w,tm2,tm1)
      def precedenceCmp(tm1: Term[V,F], tm2: Term[V,F]) =
        (tm1,tm2) match {
          case (fun1@Fun(_,args1), fun2@Fun(_,args2)) =>
            ordFun(fun1,fun2) match {
              case Ordering.LT => Some(Ordering.LT)
              case Ordering.EQ =>
                (args1.view zip args2.view find {
                  case (l,r) => l != r
                }) match {
                  case Some((l,r)) => weightCmp(l,r)
                  case None        => throw new Error("Bug: precedenceCmp")
                }
              case Ordering.GT => Some(Ordering.GT)
            }
          case (_,_) => throw new Error("Bug: precedenceCmp")
        }
      def precedenceLess(tm1: Term[V,F], tm2: Term[V,F]): Boolean =
        (tm1,tm2) match {
          case (fun1@Fun(_,args1), fun2@Fun(_,args2)) =>
            ordFun(fun1,fun2) match {
              case Ordering.LT => true
              case Ordering.EQ =>
                (args1.view zip args2.view find {
                  case (l,r) => l != r
                }) match {
                  case Some((l,r)) => weightLess(l,r)
                  case None        => throw new Error("Bug: precdenceLess")
                }
              case Ordering.GT => false
            }
          case (_,_) => false
        }
      if (this == otherTm)
        Some(Ordering.EQ)
      else
        weightCmp(this.tm,otherTm.tm)
    }
  }
  def tryCompare(tm1: Term[V,F], tm2: Term[V,F]) =
      WTerm(tm1).kbOrder(WTerm(tm2))
}

object KnuthBendix {
  def kbo[V,F](implicit
  ordInt: Order[Int], ordV: Order[V], ordF: Order[F], ordFun: Order[Fun[V,F]]) = {
    new KnuthBendix[V,F](_ => 1)
  }
}
