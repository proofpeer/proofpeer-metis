package proofpeer.metis

import proofpeer.metis.util.{PartialOrder}
import proofpeer.metis.util.RichCollectionInstances._
import scala.collection.immutable._
import scala.language.higherKinds
import scalaz._
import Scalaz._

case class ActiveFactory[
  V:Order,
  F:Order,
  P:Order,
  FV,
  K<:Kernel[V,F,P],
  ITF<:IThmFactory[V,F,P,FV,K]](
  ithmF: ITF,
  litOrder: LiteralOrdering[V,F,P],
  subsumer: Subsuming[V,F,P,Int])(
  implicit ordFun: Order[Fun[V,F]], termOrd: PartialOrder[Term[V,F]]) {

  val rewriting = METISRewriting[V,F,P,FV,ithmF.kernel.type](ithmF.kernel)

  import ClauseInstances._

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
  case class Active(
    clauses:     Map[Int,ithmF.IThm],
    rewriter:    rewriting.Rewriter,
    units:       Nets.LiteralNet[F,P,ithmF.UnitIThm],
    subsume:     subsumer.Subsume,
    literals:    Nets.LiteralNet[F,P,(Literal[V,F,P],ithmF.IThm)],
    equations:   Nets.TermNet[F,(ithmF.RewriteCursor,ithmF.IThm)],
    subterms:    Nets.TermNet[F,(ithmF.kernel.TermCursor,ithmF.IThm)],
    allSubterms: Nets.TermNet[F,(Term[V,F],ithmF.IThm)]) {

    def this() {
      this(
        Map(),
        new rewriting.Rewriter,
        new Nets.LiteralNet,
        new subsumer.Subsume,
        new Nets.LiteralNet,
        new Nets.TermNet,
        new Nets.TermNet,
        new Nets.TermNet)
    }

    def addClause(ithm: ithmF.IThm) = {
      val largestLiterals  = ithm.clause.largestLiterals(litOrder)
      val largestEquations = ithmF.RewriteCursor.rewrites(ithm)
      val largestSubterms  = ithm.thm.largestSubterms(litOrder)
      val clSubterms       = ithm.thm.clause.allSubterms
      val newLiterals = largestLiterals.foldLeft(literals) {
        case (net,lit) => net.insert(lit,(lit,ithm))
        }
      val newEquations = largestEquations.foldLeft(equations) {
        case (net,eqn) => net.insert(eqn.lhs,(eqn,ithm))
      }
      val newSubterms = largestSubterms.foldLeft(subterms) {
        case (net,tm) => net.insert(tm.get,(tm,ithm))
      }
      val newAllSubterms = clSubterms.foldLeft(allSubterms) {
        case (net,tm) => net.insert(tm.get,(tm.get,ithm))
      }

      Active(
        clauses + (ithm.id → ithm),
        rewriter,
        units,
        this.subsume.insert(ithm.clause, ithm.id),
        newLiterals,
        newEquations,
        newSubterms,
        newAllSubterms)
    }

    def deduceResolutions(ithm: ithmF.IThm) =
      for (
        lit          <- ithm.clause.largestLiterals(litOrder);
        (lit2,ithm2) <- literals.unifies(lit.negate);
        resolvent    <- ithmF.resolve(lit, ithm, lit2, ithm2))
      yield resolvent

    def deduceParamodulations(ithm: ithmF.IThm) = {
      val paraWith =
        for (
          rewr        <- ithm.rewrites;
          (subterm,_) <- subterms.unifies(rewr.lhs);
          if (subterm.get match { case Var(_) => false case _ => true });
          deduced     <- ithmF.paramodulate(rewr, subterm))
        yield deduced
      val paraInto =
        if (equations.isEmpty)
          List[ithmF.IThm]()
        else
          for (
            subterm  <- ithm.thm.largestSubterms(litOrder);
            if (subterm.get match { case Var(_) => false case _ => true });
            (rewr,_) <- equations.unifies(subterm.get);
            deduced  <- ithmF.paramodulate(rewr, subterm))
          yield deduced
      paraWith ++ paraInto
    }

    // TODO: Check against original METIS. Hurd accumulates a variable "news", which
    // appears to be built out of the other literals in the unit-theorem being
    // resolved against. Why is this? There shouldn't be any other literals if it is
    // a *unit* theorem.
    def resolveUnits(ithm: ithmF.IThm) = {
      def resolve1(thm: ithmF.IThm, lit: Literal[V,F,P]):
          List[ithmF.IThm] = {
        for (
          unitIThm  <- units.matches(lit.negate);
          resolvent <- thm.resolveUnit(lit,unitIThm).toList
        )
        yield resolvent
      }
      ithm.clause.foldLeft(ithm) { case (thm,lit) =>
        resolve1(ithm,lit).headOption.getOrElse(thm)
      }
    }

    def rewrite(ithm: ithmF.IThm) =
      ithm.repeatTopDownConvRule(rewriter.rewr(ithm.id))

    def simplify(ithm: ithmF.IThm) =
      for (
        simped    <- ithm.simplify;
        rewritten <- rewrite(simped) match {
          case None       => Some(simped)
          case Some(rewr) => rewr.simplify
        };
        resolved  = resolveUnits(rewritten);
        if !subsume.isStrictlySubsumed(resolved.clause)
      )
      yield resolved
  }

  def add(active: Active, ithm: ithmF.IThm):(Active,List[ithmF.IThm]) = {
    val simpedThm = active.simplify(ithm) match {
      case None                             =>
        return (active,List())
      case Some(ithm) if ithm.isContradiction =>
        return (active,List(ithm))
      case Some(ithm) => ithm
    }
    val (active_,deductions) =
      if (simpedThm == ithm) {
        val active_  = active.addClause(ithm)
        val freshThm = ithm.freshen
        (active_,(active_.deduceResolutions(freshThm) ++
          active_.deduceParamodulations(freshThm)).toList)
      }
      else (active,List(simpedThm))

    factor(active_,deductions)
  }

  // Hurd's factor logic as writing out a list of new theorems whilst updating
  // active.
  type SWState[A] = State[Active,A]                  // State monad
  type WT[M[_],A] = WriterT[M, List[ithmF.IThm], A]  // Writer transformer
  type SW[A] = WT[SWState, A]                        // Factor monad

  // Lift get and put up to SW
  val getSW: SW[Active] = get[Active].liftM[WT]
  def modifySW(f: Active => Active): SW[Unit] = modify[Active](f).liftM[WT]
  def putSW(active: Active): SW[Unit] = put[Active](active).liftM[WT]

  def addFactorSW(ithm: ithmF.IThm): SW[Unit] =
    for (
      active <- getSW;
      newRewriter = rewriting.Equation.ofThm(ithm.thm) match {
        case Some(eqn) => active.rewriter.add(ithm.id,eqn)
        case None      => active.rewriter
      };
      newUnits = ithmF.UnitIThm.getUnit(ithm) match {
        case Some(unit@ithmF.UnitIThm(lit,_)) =>
          active.units.insert(lit,unit)
        case _ => active.units;
      };
      newSubsumer = active.subsume.insert(ithm.clause,ithm.id);
      _ <- modifySW(
        _.copy(
          rewriter = newRewriter,
          units    = newUnits,
          subsume  = newSubsumer))
    )
    yield ()

  def simplifySW(ithm: ithmF.IThm): SW[Option[ithmF.IThm]] =
    getSW.map (_.simplify(ithm))

  // Apply f to its simplification (ignoring tautologies)
  def traverseSimplifies(ithms: List[ithmF.IThm])(f: ithmF.IThm => SW[Unit]):
      SW[Unit] =
    sortUtilityWise(ithms).traverse_ { ithm =>
      simplifySW(ithm) >>= {
        case None         => ().point[SW]
        case Some(simped) => f(simped)
      }
    }

  def findRewritables(active: Active) = {
    active.clauses.values.toSet.filter { ithm =>
      active.rewrite(ithm).filter (_ != ithm).isDefined
    }
  }

  def deleteSW(ithms: Set[ithmF.IThm]): SW[Unit] = {
    val ids = ithms.map(_.id)
    modifySW { active => active.copy (
      clauses     = active.clauses.filterKeys(!ids(_)),
      units       = active.units.filter { unit => !ids(unit.ithm.id) },
      subsume     = active.subsume.filter(!ids(_)),
      literals    = active.literals.filter    { case (_,ithm) => !ids(ithm.id) },
      equations   = active.equations.filter   { case (_,ithm) => !ids(ithm.id) },
      subterms    = active.subterms.filter    { case (_,ithm) => !ids(ithm.id) },
      allSubterms = active.allSubterms.filter { case (_,ithm) => !ids(ithm.id) })
    }
  }

  def factor(
    initActive: Active,
    ithms: List[ithmF.IThm]): (Active, List[ithmF.IThm]) = {

    def fact(ithms: List[ithmF.IThm]): SW[Unit] = {
      // Subsumer is updated over the traversals, but then restored.
      getSW.map { active =>
        modifySW { active_ =>
          active_.copy(subsume = active.subsume) } } >>= { restore =>

        if (ithms.length == 0)
          ().point[SW]
        else {
          traverseSimplifies(ithms) { ithm =>
            traverseSimplifies(ithm::ithmF.factor(ithm)) { ithm =>
              addFactorSW(ithm) :++> List(ithm);
            }
          }       *>
          restore *>
          getSW   >>= { active_ =>
            if (active_.rewriter.isReduced)
              ().point[SW]
            else {
              val (newRewriter,newRewriteIds) = active_.rewriter.reduce
              val rewritables                 = findRewritables(active_)
              putSW(active_.copy(rewriter = newRewriter)) *>
              deleteSW(rewritables) *>
              fact(rewritables.toList) }
          }
        }
      }
    }
    fact(ithms).written.run(initActive)
  }

  def isUnitEql(thm: ithmF.IThm) =
    ithmF.UnitIThm.getUnit(thm) match {
      case Some(ithmF.UnitIThm(Literal(true,Eql(_,_)),_)) => true
      case _                                              => false
    }

  def sortUtilityWise(thms: List[ithmF.IThm]) =
    thms.sortWith {
      case (thm1,thm2) =>
        thm1.isContradiction ||
        isUnitEql(thm1) && !thm2.isContradiction ||
        thm1.clause.lits.size < thm2.clause.lits.size && !thm2.isContradiction &&
        !isUnitEql(thm2)
    }
}
