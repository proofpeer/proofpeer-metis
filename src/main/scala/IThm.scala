package proofpeer.metis

import ClauseInstances._
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/** Factory for theorems with identifiers, preserved by normalising rules.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  * @tparam S The state type used to generate fresh variables.
  * @param kernel    the logic defining the thm type
  * @param nextV     state-based generator of fresh variables
  * @param initState initial state for the fresh variable generator
  * @param litOrder  an implementation of literal ordering
  * @param factoring an implementation of factoring
  */
case class IThmFactory[V:Order,F:Order,P:Order,S,K <: Kernel[V,F,P]](
  kernel: Kernel[V,F,P],
  initState: S,
  nextV : S => (S,V),
  litOrder: LiteralOrdering[V,F,P],
  factoring: Factor[V,F,P]) {

  case class IThm private[IThmFactory] (theId: Int, thm: kernel.Thm) {
    val clause = thm.clause
    val id     = theId
    val rule   = thm.rule
    def isTautology     = thm.isTautology
    def isContradiction = thm.isContradiction
    def isUnitEql       = thm.isUnitEql
  }

  object IThmInstances {
    implicit def toClause(thm: IThm) = thm.clause
  }

  var id = 0
  def newId: Int = {
    val theId = id
    id = id + 1;
    theId
  }

  /**
    *  ------------------- axiom C
    *            C
    *
    * (Generates a new theorem id)
    */
  def axiom(cl: Clause[V,F,P]) = new IThm(newId, kernel.axiom(cl))

  /**  L ∨ C      M ∨ D
    *  ------------------ resolve L, where M is the negation of L.
    *       C ∨ D
    *
    * (Generates a new theorem id)
    */
  def resolve(
    lit1: Literal[V,F,P],
    ithm1: IThm,
    lit2: Literal[V,F,P],
    ithm2: IThm) = {
    val thm1  = ithm1.thm
    val thm2  = ithm2.thm
    val nlit2 = lit2.negate
    for (
      θ <- lit1.unify(Subst.empty[V,Term[V,F]],nlit2).headOption;
      lit1_  = lit1.subst(θ);
      nlit2_ = lit1.negate;
      thm1_ = kernel.subst(θ,thm1);
      thm2_ = kernel.subst(θ,thm2);
      if litOrder.isLargerLiteral(thm1_.clause)(lit1_);
      if litOrder.isLargerLiteral(thm2_.clause)(nlit2_);
      resolvent <- kernel.resolve(lit1_,thm1_,thm2_))
    yield new IThm(newId, resolvent)
  }

  // No support for typed vars as yet (do we care about them for pp?)
  /* ~(v = t) ∨ C
   *  ------------ expandAbbrevs
   *     C[t/v]
   *
   */
  private def expandAbbrevs(thm: kernel.Thm) = {
    val firstSubst =
      thm.clause.toIterator.map {
        case NeqLit(l,r) if l != r => l.unify(Subst.empty,r).headOption
        case _                     => None
      }.find (_.isDefined).flatten
    firstSubst.map(kernel.subst(_: Subst[V,Term[V,F]],thm))
      .getOrElse(kernel.removeIrrefl(thm))
  }

  /** Simplify: chuck out if its a tautology. Otherwise expand all abbreviations
    * until a fixpoint is reached, preserving the id.
    */
  def simplify(ithm: IThm) = {
    def simp(thm: kernel.Thm): Option[kernel.Thm] = {
      if (thm.clause.isTautology) {
        None
      }
      else {
        val simpedThm = kernel.removeSym(expandAbbrevs(thm))
        if (thm == simpedThm) {
          Some(simpedThm)
        }
        else simp(simpedThm)
      }
    }
    simp(ithm.thm).map { new IThm(ithm.id,_) }
  }

  var theState = initState

  /** Replace all variables in a theorem with fresh variables. */
  def freshen(ithm: IThm) = {
    val fvs = ithm.clause.flatMap(_.frees)
    val θ   = fvs.foldLeft(Subst.empty[V,Term[V,F]]) {
      case (θ,v) =>
        val (newState,freshVar) = nextV(theState)
        theState = newState
        θ.bind(v,Var(freshVar)).get
    }
    new IThm(ithm.id,kernel.subst(θ,ithm.thm))
  }

  /**  L ∨ C       M
    *  -------------- resolveUnit, where M is the negation of L.
    *         C
    *
    * (As a simplification rule: does not generate a new theorem id)
    */
  def resolveUnit(ithm: IThm, unit: kernel.UnitThm) =
    kernel.resolve(unit.lit, ithm.thm, unit.thm).map(new IThm(ithm.id, _))

  /** Obtain all factorings of a theorem, generating new clause ids for each. */
  def factor(ithm: IThm): List[IThm] = {
    for (
      θ <- factoring.factor(ithm.clause).toList)
    yield new IThm(ithm.id, kernel.subst(θ,ithm.thm))
  }
}
