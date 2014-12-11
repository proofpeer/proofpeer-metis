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
  val subsumer  = new Subsumer[V,F,P,Clause[V,F,P]]

  private[ActiveFactory] case class Equation(
    literal:     Literal[V,F,P],
    orientation: rewriting.Direction,
    redex:       Term[V,F])

  private[ActiveFactory] class Subterm(
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
  case class Active private (
    rewriter:    rewriting.Rewrite,
    subsume:     subsumer.Subsume,
    clauses:     Map[Int,List[ithmFactory.IThm]],
    units:       Nets.LiteralNet[F,P,ithmFactory.kernel.UnitThm],
    literals:    Nets.LiteralNet[F,P,(Literal[V,F,P],ithmFactory.IThm)],
    equations:   Nets.TermNet[F,(ithmFactory.IThm, Equation)],
    subterms:    Nets.TermNet[F,(ithmFactory.IThm, Subterm)],
    allSubterms: Nets.TermNet[F,(ithmFactory.IThm, Term[V,F])]) {

    def this() {
      this(
        new rewriting.Rewrite(),
        new subsumer.Subsume(),
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
          unitThm@ithmFactory.kernel.UnitThm(matchedPat,_)
            <- units.matches(lit.negate);
          θ <- matchedPat.patMatch(Subst.empty,lit).headOption;
          resolvent <- ithmFactory.resolveUnit(
            thm,
            ithmFactory.kernel.substUnit(θ,unitThm));
          if { System.out.println("resolved a unit!"); true }
        )
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
        ithm2 = resolveUnits(ithm);
        if !subsume.isStrictlySubsumed(ithm2.clause)
      )
      yield ithm2
    }

    def addClause(ithm: ithmFactory.IThm) = {
      // TODO: Deal with equations
      val newLiterals =
        ithm.clause.largestLiterals(litOrder).foldLeft(this.literals) {
          (net,lit) => {
            System.out.println("Adding")
            System.out.println(printClause(ithm.clause.lits))
            lit match {
              case lit_ :Literal[String,String,String] =>
                System.out.println("at " + TermPrinter.printLiteral(lit_))
            }
            net.insert(lit,(lit,ithm))
          }
        }
      new Active(
        rewriter,
        subsume.insert(ithm.clause,ithm.clause),
        clauses,
        units,
        newLiterals,
        equations,
        subterms,
        allSubterms)
    }

    def factor(thms: List[ithmFactory.IThm]): (Active,List[ithmFactory.IThm]) = {
      val sortedThms = sortUtilityWise(thms)

      val (active,_,newThms) =
        sortedThms.foldLeft(this,this.subsume,List[ithmFactory.IThm]()) {
        // Presimplify
        case ((active,subsume,newThms),thm) =>
//          System.out.println("Presimplify:")
//          printClause(thm.clause.lits)
          active.simplify(thm) match {
          case None      => //System.out.println("Discard.");
              (active,subsume,newThms)
            case Some(thm) =>
            sortUtilityWise(
              thm::ithmFactory.factor(thm))
              .foldLeft(active,subsume,newThms) {

              // Postsimplify
              case ((active,subsume,newThms_),thm) => active.simplify(thm) match {
                case None =>
                  (active,subsume,newThms_)
                case Some(simpedThm) =>
                  // TODO: Update the rewriter
                  val newUnits = simpedThm match {
                    case ithmFactory.IThm(_,ithmFactory.kernel.UnitThm(unit)) =>
                      System.out.println("Inserting unit: ")
                      printClause(simpedThm.clause)
                      units.insert(unit.lit,unit)
                    case _ => units
                  }
                  val active2 = new Active(
                    active.rewriter,
                    active.subsume,
                    active.clauses,
                    newUnits,
                    active.literals,
                    active.equations,
                    active.subterms,
                    active.allSubterms)
                  (
                    active2,
                    subsume.insert(simpedThm.clause,simpedThm.clause),
                    simpedThm::newThms_
                  )
              }
            }
        }
      }
      (active,newThms)
      // TODO: Extract rewritable (and probably make tail-recursive)
    }

    def deduceResolutions(ithm: ithmFactory.IThm) = {
      printClause(ithm.clause.lits)
      literals.unifies(ithm.clause.lits.head.negate).foreach {
        case (_,ithm2) => printClause(ithm2.clause.lits)
      }
      for (
        lit          <- ithm.clause.largestLiterals(litOrder);
        (lit2,ithm2) <- literals.unifies(lit.negate);
        resolvent    <- ithmFactory.resolve(lit, ithm, lit2, ithm2))
      yield resolvent
    }

    def add(ithm: ithmFactory.IThm):
        (Active,List[ithmFactory.IThm]) = {
      val simpedThm = simplify(ithm) match {
        case None                             =>
          return (this,List())
        case Some(ithm) if ithm.isContradiction =>
          System.out.println("DONE AND DONE!")
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
    // Debug
    def getLiterals = this.literals

    def printClause(lits:Set[Literal[V,F,P]]) = {
      lits.foreach { case lit:Literal[String,String,String] =>
        System.out.println(TermPrinter.printLiteral(lit))
      }
      System.out.println("")
    }
  }
}
