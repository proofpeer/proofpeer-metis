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
  case class TermCursor[V,F,P](
    largs:  List[Literal[V,F,P]],
    cursor: Literal.TermCursor[V,F,P],
    rargs:  List[Literal[V,F,P]])
      extends GenCursor[V,Term[V,F],Clause[V,F,P],TermCursor[V,F,P]] {

    def path = cursor.path
    override def get = cursor.get
    override def replaceWith(replacement: Term[V,F]) =
      TermCursor(largs,cursor.replaceWith(replacement),rargs)
    override def subst(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) =
      TermCursor(largs.map(_.subst(θ)),cursor.subst(θ),rargs.map(_.subst(θ)))
    override def down  = cursor.down.map(TermCursor(largs,_,rargs))
    override def up    = cursor.up.map(TermCursor(largs,_,rargs))
    override def left  = {
      if (cursor.up.isDefined)
        cursor.left.map(TermCursor(largs,_,rargs))
      else
        largs match {
          case List()      => None
          case larg::largs => larg.top.map {
            top => TermCursor(largs,top,cursor.top::rargs)
          }
        }
    }
    override def right = {
      if (cursor.up.isDefined)
        cursor.right.map(TermCursor(largs,_,rargs))
      else
        rargs match {
          case List()      => None
          case rarg::rargs => rarg.top.map {
            top => TermCursor(cursor.top::largs,top,rargs)
          }
        }
    }
    override def top   = Clause((cursor.top :: largs ++ rargs).toSet)
    def literal        = cursor.top
    def literalCursor  = cursor
  }
}

/** Clauses, implicitly a set of literals, of which the clause is their
  * disjunction. Two clauses are equal when their set of literals is equal.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  */
case class Clause[V,F,P](lits: Set[Literal[V,F,P]])
    extends GenTerm[V,Term[V,F],Clause[V,F,P]]
    with Cursored[V,Term[V,F],Clause[V,F,P],Clause.TermCursor[V,F,P]]{

  import LiteralInstances._
  import AtomInstances._

  override def frees = lits.flatMap(_.frees)
  override def freeIn(v: V) = lits.exists(_.freeIn(v))
  override def subst(θ: Subst[V,Term[V,F]])(implicit ev: Order[V]) =
    new Clause(lits.map(_.subst(θ)))
  override def heuristicSize = lits.toList.map(_.heuristicSize).sum

  override def top =
    lits.toList match {
      case List()    => None
      case lit::lits =>
        lit.top.map {
          top => Clause.TermCursor(List(),top,lits)
        }
    }

  implicit def atomOrder(implicit ordV: Order[V], ordF: Order[F], ordP: Order[P]) =
    Order[Atom[V,F,P]].toScalaOrdering

  def isTautology: Boolean = {
    lits.foldLeft(Set[Atom[V,F,P]]()) {
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
      (llits,lit,rlits) <- util.Fun.splits(lits.toList);
      if litOrder.isMaximal(this.lits)(lit);
      cursor  <- lit.allSubterms)
    yield Clause.TermCursor(llits,cursor,rlits)
  }
}

object ClauseInstances {
  implicit def toRichIterable[V,F,P](cl: Clause[V,F,P]) =
    new RichIterable(cl.lits)
  implicit def toSet[V,F,P](cl: Clause[V,F,P]) = cl.lits
  import LiteralInstances._
  implicit def ClauseIsShow[V:Show,F:Show,P:Show] = new Show[Clause[V,F,P]] {
    override def show(clause: Clause[V,F,P]) =
      Cord("Clause(") ++ Cord.mkCord(",",clause.lits.toSeq.map(_.show):_*) ++ ")"
    }
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
