package proofpeer.metis

import util.Fun._
import scala.collection.immutable._
import scalaz._
import Scalaz._

/** Substitutions of terms for variables
  *
  * @tparam V alphabet from which variables are drawn
  * @tparam T type of terms substituted
  */
case class Subst[V:Order,T](θ: V ==>> T) extends Function[V,Option[T]] {
  override def apply(v: V): Option[T] = this.lookup(v)

  def lookup(v: V) = θ.lookup(v)

  /** Attempt to add a binding. If the binding already exists, return the same.
      If a different binding to v already exists, return None. */
  def bind(v: V, tm: T): Option[Subst[V,T]] = {
    θ.lookup(v) match {
      case None                   => Some(Subst(θ + { v → tm }))
      case Some(rhs) if tm == rhs => Some(Subst(θ))
      case _                      => None
    }
  }

  def mapRhs[U](f: T => U): Subst[V,U] = {
    Subst(θ.map(f))
  }

  def union(σ: Subst[V,T]) : Option[Subst[V,T]] = {
    Some(Subst((θ unionWithKey σ.θ) {
      case (v,tm1,tm2) => if (tm1 == tm2) tm1 else return None
    }))
  }
}

object Subst {
  def empty[V:Order,T] = Subst(==>>.empty[V,T])
}

/** General terms, abstracting over first-order terms and formulas.
  *
  * @tparam V The alphabet from which variable names are drawn.
  * @tparam T type of terms substituted
  * @tparam C type of subterm cursors
  * @tparam GT concrete type
  * @tparam type of subterms
  */
trait GenTerm[V,T,GT] { this: GT =>
  def frees(implicit ev: Order[V]): ISet[V]
  def freeIn(v: V): Boolean
  def subst(θ: V => Option[T])(implicit ev: Order[V]): GT

  def heuristicSize: Int
}

trait MatchableTerm[V,T,GT] { this: GT =>
  /** Treat this term as a pattern and match it against term. */
  def patMatch(θ: Subst[V,T], term: GT)(implicit ev: Order[V]): Option[Subst[V,T]]
  def unify(θ: Subst[V,T], term: GT)(implicit ev: Order[V]): Option[Subst[V,T]]
}

trait Cursored[V,T,GT,C <: GenCursor[V,T,GT,C]] { this: GT =>
  def topLeft: Option[C]

  def allSubterms: List[C] =
    for (
      t   <- topLeft.toList;
      sib <- t :: t.siblings;
      st  <- sib.allSubterms
    )
    yield st
}

trait GenCursor[V,T,GT,C <: GenCursor[V,T,GT,C]] { this: C =>
  /** The term under the cursor. */
  def get: T

  /** The term under the top cursor */
  def top: GT

  def up: Option[C]

  def down: Option[C]

  def left: Option[C]

  def right: Option[C]

  def children: List[C] = {
    down.toList >>= (loopCollect(_)(c => c.right))
  }

  def siblings: List[C] =
    loopCollect(this)(_.left) ++ loopCollect(this)(_.right)

  def path: Vector[Int]

  /** Replace the term under the cursor. */
  def replaceWith(T: T): C

  def subst(θ: V => Option[T])(implicit ev: Order[V]): C

  def allSubterms: List[C] =
    this :: (for (
      child <- children;
      c     <- child.allSubterms)
    yield c)
}
