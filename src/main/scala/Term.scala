package proofpeer.metis

import proofpeer.metis.util.{PartialOrder}
import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._

object Term {
  case class TermCursor[V,F] private[Term] (
    top: Term[V,F],
    cursorTerm: Term[V,F],
    path: List[Int]) extends GenCursor[V,Term[V,F],Term[V,F],TermCursor[V,F]] {

    override def get = cursorTerm

    private def replaceAt(
      term: Term[V,F],
      replacement: Term[V,F],
      path: List[Int]): Term[V,F] = {
      (term,path) match {
        case (_,List())           => replacement
        case (Fun(f,args), n::ns) =>
          args.splitAt(n) match {
            case (pre,arg::sucs) =>
              Fun(f,pre ++ (replaceAt(arg,replacement,ns)::sucs))
            case _               =>
              throw new Error("Bug: No such subterm.")
          }
        case _ => throw new Error("Bug: No such subterm.")
      }
    }

    override def replaceWith(replacement: Term[V,F]): Term[V,F] =
      replaceAt(this.top,replacement,this.path)

    override def substTop(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) =
      new TermCursor(top.subst(θ),cursorTerm.subst(θ),path)
  }
}

/** Terms as in first-order logic; i.e. the structured elements that are arguments to
  * predicates and that appear on the left- and right-hand sides of equations.
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  */
abstract sealed class Term[V,F]
    extends GenTerm[V,Term[V,F],Term.TermCursor[V,F],Term[V,F]] {

  override def freeIn(v: V): Boolean = {
    this match {
      case Var(v_) => v == v_
      case Fun(_,args) => args.exists(_.freeIn(v))
    }
  }

  override def frees: Set[V] = {
    this match {
      case Fun(_,args) =>
        args.foldLeft(Set[V]()) {
          case (fvs,arg) => fvs union arg.frees
        }
      case Var(v)      => Set(v)
    }
  }

  override def patMatch(θ: Subst[V,Term[V,F]],term: Term[V,F])(
    implicit ev: Order[V]):
      List[Subst[V,Term[V,F]]] = {
    (this,term) match {
      case (Var(v),tm) => θ.bind(v,tm).toList
      case (Fun(f1,args1), Fun(f2,args2))
          if f1 == f2 && args1.length == args2.length =>
        (args1 zip args2).foldLeftM(θ) {
          (θ, args) => {
            args match {
              case (arg1,arg2) => arg1.patMatch(θ,arg2)
            }
          }
        }
      case _ => List()
    }
  }

  override def unify(θ: Subst[V,Term[V,F]],otherTerm: Term[V,F])(implicit
      ev: Order[V]):
      List[Subst[V,Term[V,F]]] = {
    (this,otherTerm) match {
      case (Var(v1),Var(v2)) if v1 == v2 => List(θ)
      case (Var(v),_) =>
        θ.lift(v) match {
          case None =>
            val otherTerm_ = otherTerm.subst(θ)
            if (this == otherTerm_)
              List(θ)
            else if (otherTerm_.freeIn(v))
              List()
            else {
              val vMapping = Subst.empty[V,Term[V,F]].bind(v,otherTerm_).get
              val θ2 = θ.mapRhs { _.subst(vMapping) }
              List(θ2.bind(v,otherTerm_).get)
            }
          case Some(bndTerm) => bndTerm.unify(θ,otherTerm)
        }
      case (_,v@Var(_)) => v.unify(θ,this)
      case (Fun(f1,args1),(Fun(f2,args2)))
          if f1 == f2 && args1.length == args2.length =>
        (args1 zip args2).foldLeftM(θ) {
          case (θ, (arg1,arg2)) => arg1.unify(θ,arg2)
        }
      case _ => List()
    }
  }

  // Removed optimisations from the SML:
  //   * In Hurd's, a substitution is a map and can be checked for emptiness, in
  //     which case the term is not traversed. We could go with this, but would have
  //     to use a more specific type than PartialFunction to represent θ.
  //   * As in much of the HOL Light kernel code, terms are not reconstructed if
  //     the constructor arguments are pointer-equal. Could go for this without
  //     having to change any types.
  override def subst(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) = {
    def sub(term: Term[V,F]): Term[V,F] = {
      term match {
        case Var(v)      => θ.lift(v).getOrElse(term)
        case Fun(f,args) => Fun(f,args.map(sub))
      }
    }
    sub(this)
  }

  // Optimise: make tail-recursive
  override def heuristicSize: Int = {
    this match {
      case Var(x)      => 1
      case Fun(f,args) => 1 + args.map(_.heuristicSize).sum
    }
  }

  override def allSubterms: List[Term.TermCursor[V,F]] = {
    new Term.TermCursor(this,this,List()) :: (this match {
      case Fun(_,args) =>
        for (
          (arg,i) <- args.zipWithIndex;
          cursor  <- arg.allSubterms)
        yield new Term.TermCursor(this,cursor.get,i::cursor.path)
      case _           => List()
    })
  }
}

case class Var[V,F](v: V) extends Term[V,F]
case class Fun[V,F](f: F, args: List[Term[V,F]]) extends Term[V,F]

object TermInstances {
  implicit def ordTerm[V:Order,F:Order]: Order[Term[V,F]] =
    new Order[Term[V,F]]{
      def order (x:Term[V,F], y:Term[V,F]): Ordering =
        (x,y) match {
          case (Var(v1),Var(v2))  => v1 ?|? v2
          case (Var(_), Fun(_,_)) => Ordering.LT
          case (Fun(_,_), Var(_)) => Ordering.GT
          case (Fun(f1,args1),Fun(f2,args2)) =>
            (args1.length,f1,args1) ?|? (args2.length,f2,args2)
        }
    }

  implicit def TermIsMonad[F] = new Monad[({type λ[V] = Term[V,F]})#λ] {
    override def point[V](x: => V) = Var[V,F](x)
    override def bind[U,V](tm: Term[U,F])(f: U => Term[V,F]): Term[V,F] = {
      tm match {
        case Var(v)       => f(v)
        case Fun(fn,args) => Fun[V,F](fn,args map (bind(_)(f)))
      }
    }
  }

  implicit def TermIsShow[V:Show,F:Show] = new Show[Term[V,F]] {
    override def show(tm: Term[V,F]): Cord = {
      tm match {
      case Var(v) => v.show
      case Fun(f,List()) => f.show
      case Fun("Multiply",List(x,y)) =>
        Cord("(") ++ show(x) ++ " * " ++ show(y) ++ Cord(")")
      case Fun(f,args) => f.show ++
          Cord("(") ++ Cord.mkCord(",",args.map(show(_)):_*)
        ")"
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

  def fromTerm[V:Order,F](
    tm: Term[V,F],
    funWeight: (F,Int) => Int)(implicit
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
        case (Fun(f,args))::tms =>
          wt(Weight(
            w.nameMap,
            w.c + funWeight(f,args.length)),args ++ tms)
      }
    implicit val ord = ordV.toScalaOrdering
    wt(Weight(Map[V,Int](),-1),List(tm))
  }
}

/** The Knuth Bendix term order.
  *
  * @param funWeight An arbitrary weighting of functor names. Defaults to a constant
  * function in METIS.
  */
class KnuthBendix[V:Order,F:Order](funWeight: (F,Int) => Int)(
  implicit ordFun: Order[Fun[V,F]]) extends PartialOrder[Term[V,F]] {

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
  override def tryCompare(tm1: Term[V,F], tm2: Term[V,F]) =
    WTerm(tm1).kbOrder(WTerm(tm2))
}

object KnuthBendix {
  def kbo[V,F](implicit
  ordInt: Order[Int], ordV: Order[V], ordF: Order[F], ordFun: Order[Fun[V,F]]) = {
    new KnuthBendix[V,F]( { case (_,_) => 1 } )
  }

  def precedenceOrder[V,F:Order] = new Order[Fun[V,F]] {
    def order(f: Fun[V,F], g: Fun[V,F]) = {
      (f.args.length,f.f) ?|? (g.args.length,g.f)
    }
  }
}
