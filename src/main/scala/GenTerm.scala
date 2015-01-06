package proofpeer.metis

import scala.collection.immutable._
import scalaz._
import Scalaz._

/** Substitutions of terms for variables
  * @tparam V alphabet from which variables are drawn
  * @tparam T type of terms substituted for
  */
case class Subst[V:Order,T] private(θ: V ==>> T) extends PartialFunction[V,T] {
  override def isDefinedAt(v: V) = θ.member(v)
  override def apply(v: V): T = this.lookup(v).get

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
  * @tparam V The alphabet from which variable names are drawn. */
trait GenTerm[V,T,GT] { this: GT =>
  def frees: Set[V]
  def freeIn(v: V): Boolean
  def subst(θ: Subst[V,T]): GT

  def heuristicSize: Int

  /** Treat this term as a pattern and match it against term. */
  def patMatch(θ: Subst[V,T], term: GT): List[Subst[V,T]]

  def unify(θ: Subst[V,T], term: GT): List[Subst[V,T]]
}