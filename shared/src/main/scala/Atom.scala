package proofpeer.metis

import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import TermInstances._

object Atom {
  sealed trait TermCursor[V,F,P]
      extends GenCursor[V,Term[V,F],Atom[V,F,P],TermCursor[V,F,P]] {
    def path: Vector[Int]
  }

  case class PredCursor[V,F,P] private[Atom](
    top: Pred[V,F,P],
    pos: Int,
    cursor: Term.TermCursor[V,F]) extends TermCursor[V,F,P] {

    /** Argument path from theTop to the cursor. */
    def path = pos +: cursor.path

    override def get = cursor.get
    override def replaceWith(replacement: Term[V,F]): Pred[V,F,P] = {
      top.args.splitAt(pos) match {
        case (pre,arg::sucs) =>
          Pred(top.functor,pre ++ (cursor.replaceWith(replacement)::sucs))
        case _ => throw new Error("Bug: No such subterm.")
      }
    }

    override def substTop(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) = {
      val cursor_ = cursor.substTop(θ)
      val atm_    = cursor_.top
      top.args.splitAt(pos) match {
        case (pre,_::sucs) =>
          val atm = Pred(
            top.functor,pre.map(_.subst(θ)) ++
              (atm_ ::
                sucs.map(_.subst(θ))))
          new PredCursor(atm,pos,cursor_)
        case _ => throw new Error("Bug: No such subterm.")
      }
    }

    override def children =
      this.cursor.children.map(new PredCursor(this.top,this.pos,_))

  }

  case class LHSCursor[V,F,P] private[Atom](
    top: Eql[V,F,P],
    cursor: Term.TermCursor[V,F]) extends TermCursor[V,F,P] {

    override def path = 0 +: cursor.path
    override def get = cursor.get
    override def replaceWith(replacement: Term[V,F]): Eql[V,F,P] =
      Eql(cursor.replaceWith(replacement),top.r)

    override def substTop(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) = {
      val cursor_ = cursor.substTop(θ)
      val lhs_    = cursor_.top
      val rhs_    = top.r.subst(θ)
      new LHSCursor(Eql[V,F,P](lhs_,rhs_),cursor_)
    }

    override def children =
      this.cursor.children.map(new LHSCursor(this.top,_))
  }

  case class RHSCursor[V,F,P] private[Atom](
    top: Eql[V,F,P],
    cursor: Term.TermCursor[V,F]) extends TermCursor[V,F,P] {
    override def path = 1 +: cursor.path
    override def get = cursor.get
    override def replaceWith(replacement: Term[V,F]): Eql[V,F,P] =
      Eql(top.l,cursor.replaceWith(replacement))

    override def substTop(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) = {
      val cursor_ = cursor.substTop(θ)
      val lhs_    = top.l.subst(θ)
      val rhs_    = cursor_.top
      new RHSCursor(Eql[V,F,P](lhs_,rhs_),cursor_)
    }
    override def children =
      this.cursor.children.map(new RHSCursor(this.top,_))
  }
}

/** Atomic formulas for first-order logic with equality.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  */
abstract sealed class Atom[V,F,P]
    extends GenTerm[V,Term[V,F],Atom.TermCursor[V,F,P],Atom[V,F,P]] {

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

  override def patMatch(θ: Subst[V,Term[V,F]], atm: Atom[V,F,P])(
    implicit ev: Order[V]):
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

  override def unify(θ: Subst[V,Term[V,F]], atm: Atom[V,F,P])(implicit ev: Order[V]):
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

  override def tops = {
    this match {
      case eq@Eql(x,y) =>
        x.tops.map { new Atom.LHSCursor(eq,_) } ++
        y.tops.map { new Atom.RHSCursor(eq,_) }
      case atm@Pred(p,args) =>
        for (
          (arg,n) <- args.zipWithIndex;
          cursor  <- arg.tops)
        yield new Atom.PredCursor(atm,n,cursor)
    }
  }

  def isRefl = this match {
    case Eql(l,r) => l == r
    case _        => false
  }

  def lhs: Option[Atom.TermCursor[V,F,P]] =
    this match {
      case eq@Eql(x,_) => Some(new Atom.LHSCursor(eq,x.cursor))
      case _ => None
    }
  def rhs: Option[Atom.TermCursor[V,F,P]] =
    this match {
      case eq@Eql(_,y) => Some(new Atom.LHSCursor(eq,y.cursor))
      case _ => None
    }
}

/** Predications P(...args...) */
case class Pred[V,F,P](functor: P, args: List[Term[V,F]]) extends Atom[V,F,P] {
  override def subst(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]): Pred[V,F,P] =
    Pred(functor,args.map(_.subst(θ)))
}

/** Equations */
case class Eql[V,F,P](l: Term[V,F], r: Term[V,F]) extends Atom[V,F,P] {
  override def subst(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]): Eql[V,F,P] =
    Eql(l.subst(θ),r.subst(θ))
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

  implicit def AtomIsShow[V:Show,F:Show,P:Show] = new Show[Atom[V,F,P]] {
    override def show(atom: Atom[V,F,P]) = atom match {
      case Eql(x,y)       => x.show ++ Cord("=") ++ y.show
      case Pred(p,List()) => p.show
      case Pred(p,args)   =>
        p.show ++
        Cord("(") ++ Cord.mkCord(",",args.map(_.show):_*) ++ ")"
    }
  }
}
