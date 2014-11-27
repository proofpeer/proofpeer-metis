package proofpeer.metis

import scala.collection.immutable._
import scalaz._

case class ActiveFactory[
  V,F,P,S,
  K <: Kernel[V,F,P],
  ITF <: IThmFactory[V,F,P,S,K]](
  ithmFactory: ITF,
  litOrder: LiteralOrdering[V,F,P])(implicit
  ordInt: Order[Int],
  ordV: Order[V],
  ordF: Order[F],
  ordFun: Order[Fun[V,F]]) {

  import ClauseInstances._

  val rewriting = new Rewriting[V,F,P,ithmFactory.kernel.type](ithmFactory.kernel)

  private case class Equation(
    literal:     Literal[V,F,P],
    orientation: rewriting.Direction,
    redex:       Term[V,F])

  private class Subterm(
    literal:     Literal[V,F,P],
    path:        Term.Path,
    subterm:     Term[V,F])

  // units: map from a literal to a unit theorem of that literal. Used for fast
  // resolution.
  // literals: map from a literal to a theorem containing that literal. Used for
  // resolution.
  // equations: map from a redex to its corresponding (unit) equation, giving
  // the orientation (left-to-right means the redex is on the left, and right
  // otherwise)
  // subterms: map from a subterm to the literal and the theorem containing that
  // literal where the subterm occurs at the given path.
  // allSubterms: map from a subterm to a theorem containing that subterm somewhere.
  // Used to check if an equation can be used for some rewriting.
  class Active private (
    rewriter:    rewriting.Rewrite,
    clauses:     Map[Int,List[ithmFactory.IThm]],
    units:       Nets.LiteralNet[F,P,ithmFactory.kernel.UnitThm],
    literals:    Nets.LiteralNet[F,P,(Literal[V,F,P],ithmFactory.IThm)],
    equations:   Nets.TermNet[F,(ithmFactory.IThm, Equation)],
    subterms:    Nets.TermNet[F,(ithmFactory.IThm, Subterm)],
    allSubterms: Nets.TermNet[F,(ithmFactory.IThm, Term[V,F])]) {

    def this() {
      this(
        new rewriting.Rewrite(),
        Map(),
        new Nets.LiteralNet,
        new Nets.LiteralNet,
        new Nets.TermNet(),
        new Nets.TermNet(),
        new Nets.TermNet())
    }

    // TODO: Check against original METIS. Hurd accumulates a variable "news", which
    // appears to be built out of the other literals in the unit-theorem being
    // resolved against. Why is this? There shouldn't be any other literals if it is
    // a *unit* theorem.
    private def resolveUnits(ithm: ithmFactory.IThm) = {
      def resolve1(thm: ithmFactory.IThm, lit: Literal[V,F,P]): ithmFactory.IThm = {
        for (
          unitThm@ithmFactory.kernel.UnitThm(matchedPat,_) <- units.matches(lit);
          θ <- matchedPat.patMatch(Subst.empty,lit).headOption;
          resolvent <- ithmFactory.resolveUnit(
            thm,
            ithmFactory.kernel.substUnit(θ,unitThm)))
          return resolvent
        return thm
      }
      ithm.clause.foldLeft(ithm) { case (thm,lit) =>
        resolve1(ithm,lit)
      }
    }

    private def sortUtilityWise(thms: List[ithmFactory.IThm]) = {
      thms.sortWith {
        case (thm1,thm2) =>
          thm1.isContradiction || !thm2.isContradiction ||
          thm1.isUnitEql || !thm2.isUnitEql ||
          (thm1.clause.size < thm2.clause.size)
      }
    }

    def simplify(ithm: ithmFactory.IThm) = {
      for (
        ithm <- ithmFactory.simplify(ithm);
        // TODO: Rewrite
        ithm2 = resolveUnits(ithm)
        // TODO: Subsumption
      )
      yield ithm
    }

    def addClause(ithm: ithmFactory.IThm) = {
      // TODO: Deal with equations
      val newLiterals =
        ithm.clause.largestLiterals(litOrder).foldLeft(this.literals) {
          (net,lit) => net.insert(lit,(lit,ithm))
        }
      new Active(
        rewriter,
        clauses,
        units,
        newLiterals,
        equations,
        subterms,
        allSubterms)
    }

    def factor(thms: List[ithmFactory.IThm]): (Active,List[ithmFactory.IThm]) = {
      val sortedThms = sortUtilityWise(thms)

      val (active,newThms) = sortedThms.foldLeft(this,List[ithmFactory.IThm]()) {
        // Presimplify
        case ((active,newThms),thm) => active.simplify(thm) match {
          case None      => (active,newThms)
          case Some(thm) =>
            sortUtilityWise(
              thm::ithmFactory.factor(thm))
              .foldLeft(active,List[ithmFactory.IThm]()) {
              // Postsimplify
              case ((active,newThms),thm) => active.simplify(thm) match {
                case None => (active,newThms)
                case Some(simpedThm@ithmFactory.IThm(
                  id,
                  ithmFactory.kernel.UnitThm(unit))) =>
                  val active2 = new Active(
                    rewriter,
                    clauses,
                    units.insert(unit.lit,unit),
                    literals,
                    equations,
                    subterms,
                    allSubterms)
                  // TODO: Update subsumer
                  (active2, simpedThm::newThms)
                case Some(simpedThm) => (active,simpedThm::newThms)
              }
            }
        }
      }
      (active,newThms)
      // TODO: Extract rewritable (and probably make tail-recursive)
    }

    def deduceResolution1(lit:Literal[V,F,P], ithm: ithmFactory.IThm) = {
      for (
        (lit2,ithm2) <- literals.unifies(lit.negate);
        resolvent    <- ithmFactory.resolve(lit, ithm, lit2, ithm2))
      yield resolvent
    }

    def deduceResolutions(ithm: ithmFactory.IThm) = {
      ithm.clause.largestLiterals(litOrder).flatMap {
        deduceResolution1(_, ithm)
      }
    }

    def add(ithm: ithmFactory.IThm):
        (Active,List[ithmFactory.IThm]) = {

      val simpedThm = simplify(ithm) match {
        case None                             =>
          return (this,List(ithm))
        case Some(ithm) if ithm.isContradiction =>
          return (this,List(ithm))
        case Some(ithm) => ithm
      }

      if (simpedThm == ithm) {
        val active   = addClause(ithm)
        val freshThm = ithmFactory.freshen(ithm)
        val derived  = active.deduceResolutions(freshThm)
        active.factor(derived.toList)
      }
      else factor(List(simpedThm))
    }
  }
}
