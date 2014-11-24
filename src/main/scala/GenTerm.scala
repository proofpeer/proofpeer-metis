package proofpeer.metis

import scala.collection.immutable._
import scalaz._
import Scalaz._

/** Substitutions of terms for variables
  * @tparam V alphabet from which variables are drawn
  * @tparam T type of terms substituted for
  */
case class Subst[V,T] private(θ: Map[V,T]) extends PartialFunction[V,T] {
  def isDefinedAt(v: V) = θ.isDefinedAt(v)
  def apply(v: V) = θ(v)

  /** Attempt to add a binding. If the binding already exists, return the same.
      If a different binding to v already exists, return None. */
  def bind(v: V, tm: T): Option[Subst[V,T]] = {
    θ.get(v) match {
      case None                   => Some(Subst(θ + { v → tm }))
      case Some(rhs) if tm == rhs => Some(Subst(θ))
      case _                      => None
    }
  }

  def map[U](f: T => U): Subst[V,U] = {
    Subst(θ.mapValues(f))
  }
}

object Subst {
  def empty[V,T](implicit ordV: Order[V]) = {
    implicit val _ = ordV.toScalaOrdering
    Subst(TreeMap[V,T]())
  }
}

/** General terms, abstracting over first-order terms and formulas.
  * @tparam V The alphabet from which variable names are drawn. */
trait GenTerm[V,T,GT] { this: GT =>
  def frees: Set[V]
  def freeIn(v: V): Boolean
  def subst(θ: Subst[V,T]): GT

  /** Treat this term as a pattern and match it against term. */
  def patMatch(θ: Subst[V,T], term: GT): List[Subst[V,T]]

  def unify(θ: Subst[V,T], term: GT): List[Subst[V,T]]
}
