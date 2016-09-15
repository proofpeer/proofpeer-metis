package proofpeer.metis

import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import TermInstances._

object Atom {
  sealed trait TermCursor[V,F,P]
      extends GenCursor[V,Term[V,F],Atom[V,F,P],TermCursor[V,F,P]]

  case class PredCursor[V,F,P] private[Atom](
    p: P,
    largs: List[Term[V,F]],
    cursor: Term.TermCursor[V,F],
    rargs: List[Term[V,F]]) extends TermCursor[V,F,P] {

    /** Argument path from theTop to the cursor. */
    override def path = largs.length +: cursor.path

    override def get = cursor.get
    override def replaceWith(replacement: Term[V,F]) =
      PredCursor(p,largs,cursor.replaceWith(replacement),rargs)

    override def subst(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]): TermCursor[V,F,P] =
      PredCursor(p,largs.map(_.subst(θ)), cursor.subst(θ), rargs.map(_.subst(θ)))

    override def down  = cursor.down.map(PredCursor(p,largs,_,rargs))
    override def left  = {
      if (cursor.up.isDefined)
        cursor.left.map(PredCursor(p,largs,_,rargs))
      else
        largs match {
          case List()      => None
          case larg::largs =>
            larg.topLeft.map { top => PredCursor(p,largs,top,cursor.top::rargs) }
        }
    }
    override def right = {
      if (cursor.up.isDefined)
        cursor.right.map(PredCursor(p,largs,_,rargs))
      else
        rargs match {
          case List()      => None
          case rarg::rargs =>
            rarg.topLeft.map { top => PredCursor(p,cursor.top::largs,top,rargs) }
        }
    }
    override def up    = cursor.up.map(PredCursor(p,largs,_,rargs))
    override def top   = Pred(p,largs.reverse ++ List(cursor.top) ++ rargs)
  }

  case class LHSCursor[V,F,P] private[Atom](
    cursor: Term.TermCursor[V,F],
    rhs: Term[V,F]) extends TermCursor[V,F,P] {

    override def path = 0 +: cursor.path
    override def get = cursor.get
    override def replaceWith(replacement: Term[V,F]) =
      LHSCursor(cursor.replaceWith(replacement),rhs)
    override def subst(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) =
      new LHSCursor(cursor.subst(θ),rhs.subst(θ))
    override def down  = cursor.down.map(LHSCursor(_,rhs))
    override def left  = cursor.left.map(LHSCursor(_,rhs))
    override def right =
      if (cursor.up.isDefined)
        cursor.right.map(LHSCursor(_,rhs))
      else rhs.topLeft.map { RHSCursor(cursor.top,_) }
    override def up    = cursor.up.map(LHSCursor(_,rhs))
    override def top   = Eql(cursor.top,rhs)
  }

  case class RHSCursor[V,F,P] private[Atom](
    lhs: Term[V,F],
    cursor: Term.TermCursor[V,F]) extends TermCursor[V,F,P] {
    override def path = 1 +: cursor.path
    override def get = cursor.get
    override def replaceWith(replacement: Term[V,F]) =
      RHSCursor(lhs,cursor.replaceWith(replacement))
    override def subst(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) =
      RHSCursor(lhs.subst(θ),cursor.subst(θ))
    override def down  = cursor.down.map(RHSCursor(lhs,_))
    override def left  =
      if (cursor.up.isDefined)
        cursor.left.map(RHSCursor(lhs,_))
      else lhs.topLeft.map(LHSCursor(_,cursor.top))
    override def right = cursor.right.map(RHSCursor(lhs,_))
    override def up    = cursor.up.map(RHSCursor(lhs,_))
    override def top   = Eql(lhs,cursor.top)
  }
}

/** Atomic formulas for first-order logic with equality.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  */
abstract sealed class Atom[V,F,P]
    extends GenTerm[V,Term[V,F],Atom[V,F,P]]
    with MatchableTerm[V,Term[V,F],Atom[V,F,P]]
    with Cursored[V,Term[V,F],Atom[V,F,P],Atom.TermCursor[V,F,P]] {

  override def frees(implicit ev: Order[V]) = this match {
    case Pred(_,args) => args.foldMap { _.frees }
    case Eql(l,r) => l.frees union r.frees
  }

  override def freeIn(v: V) = this match {
    case Pred(_,args) => args.exists(_.freeIn(v))
    case Eql(l,r)     => l.freeIn(v) || r.freeIn(v)
  }

  override def patMatch(θ: Subst[V,Term[V,F]], atm: Atom[V,F,P])(implicit ev: Order[V]):
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

  override def topLeft = {
    this match {
      case eq@Eql(x,y) =>
        x.topLeft.map(Atom.LHSCursor[V,F,P](_,y))
      case Pred(p,List())    => None
      case Pred(p,arg::args) =>
        arg.topLeft.map { top => Atom.PredCursor(p,List(),top,args) }
    }
  }

  def isRefl = this match {
    case Eql(l,r) => l == r
    case _        => false
  }
}

/** Predications P(...args...) */
case class Pred[V,F,P](functor: P, args: List[Term[V,F]])
    extends Atom[V,F,P] {
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
