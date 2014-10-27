package proofpeer.metis

import scalaz._
import Scalaz._

/** Derived rules */
/** A set of derived rules based on orderings of the three alphabets.  */
case class Rules[V,F,P](kernel: Thm.Kernel[V,F,P])(implicit
  ordV: Order[V],
  ordF: Order[F],
  ordP: Order[P]) {
  private val k = kernel

  /**   x = y ∨ C
    *  ------------ symRule x y
    *   y = x v C
    */
  def symRule(x: Term[V,F], y: Term[V,F], thm: Thm[V,F,P]) = {
    val sym = k.sym(x,y)
    k.resolve(Literal(true,Eql(x,y)),thm,sym)
  }

  // No support for typed vars as yet (do we care about them for pp?)
  /** ~(v = t) ∨ C
    *  ------------ expandAbbrevs
    *     C[t/v]
    */
  def expandAbbrevs(thm: Thm[V,F,P]) = {
    val firstSubst =
      thm.clause.toIterator.map {
        case NeqLit(l,r) if l != r => Term.termUnify(PartialFunction.empty,l,r)
        case _                     => None
      }.find (_.isDefined).flatten
    firstSubst.map(k.subst(_: Term.Subst[V,F],thm)).getOrElse(k.removeIrrefl(thm))
  }

  /** Simplify: chuck out if its a tautology. Otherwise expand all abbreviations
    * until a fixpoint is reached.
    *
    */
  def simplify(thm: Thm[V,F,P]): Option[Thm[V,F,P]] = {
    if (thm.isTautology) {
      None
    }
    else {
      if (thm == k.removeSym(expandAbbrevs(thm))) {
        Some(thm)
      }
      else simplify(thm)
    }
  }
}
