package proofpeer.metis

import ClauseInstances._
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/** A logical kernel for trusted resolution certificates.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
*/
sealed class Kernel[V,F,P](implicit
  ordV: Order[V],
  ordF: Order[F],
  ordP: Order[P]) {

  sealed abstract class Inference
  case class Axiom() extends Inference
  case class Assume() extends Inference
  case class Refl() extends Inference
  case class Sym() extends Inference
  case class Trans[V,F,P](xy: Thm, yz: Thm) extends Inference
  case class Rewrite[V,F,P](p: Term.Path, eq:Thm) extends Inference
  case class RemoveSym[V,F,P](thm: Thm) extends Inference
  case class Conv[V,F,P](thm: List[Thm]) extends Inference
  case class InfSubst(θ: Subst[V,Term[V,F]]) extends Inference
  case class Factor() extends Inference
  case class Resolve[V,F,P](pos: Thm, neg: Thm) extends Inference
  case class Equality() extends Inference
  case class Irreflexive[V,F,P](thm: Thm) extends Inference

  case class Thm private[Kernel](clause: Clause[V,F,P], rule: Inference) {
    def isTautology     = clause.isTautology
    def isContradiction = clause.isContradiction
    def isUnitEql       = clause.isUnitEql
  }

  /**
    *  ------------------- axiom C
    *            C
    */
  def axiom(cl: Clause[V,F,P]): Thm = Thm(cl, Axiom())

  /**
    *  ------------------- assume p
    *        p ∨ ¬p
    */
  def assume(lit: Literal[V,F,P]) = Thm(Clause(Set(lit,lit.negate)), Assume())

  /**
    *  ------------------- refl x
    *         x = x
    */
  def refl(tm: Term[V,F]) = Thm(Clause(Set(Literal(true,Eql(tm,tm)))),Refl())

  /**  L ∨ C      M ∨ D
    *  ------------------ resolve L, where M is the negation of L.
    *       C ∨ D
    */
  def resolve(lit: Literal[V,F,P], thm1: Thm, thm2: Thm):
      Option[Thm] = {
    val negLit = lit.negate
    // Could push check onto caller for possible optimisation
    if (thm1.clause.contains(lit) && thm2.clause.contains(negLit)) {
      val cl = (thm1.clause - lit) ++ (thm2.clause - negLit)
      Some(Thm(Clause(cl), Resolve(thm1,thm2)))
    }
    else None
  }

  /**     C
    *  ------- subst θ
    *    C[θ]
    */
  def subst(θ: Subst[V,Term[V,F]], thm: Thm): Thm =
    // For now, removing optimisation where, if the entire clause is unchanged, we
    // do not return a newly constructed clause.
    new Thm(Clause(thm.clause.subst(θ)),InfSubst(θ))

  // Derived rules in Hurd. Primitive here.
  // ======================================

  /** ---------------------- sym x y
    *    ¬(x = y) ∨ (y = x)
    */
  def sym(x: Term[V,F], y: Term[V,F]) = {
    val cl = Clause(Set(Literal(false,Eql[V,F,P](x,y)),Literal(true,Eql[V,F,P](y,x))))
    Thm(Clause(cl),Sym())
  }

  /**      C ∨ ~(x = x)
    *  ------------------- removeIrrefl
    *           C
    */
  def removeIrrefl(thm: Thm): Thm = {
    val newCl = thm.clause.filter {
        case IrreflLit(_) => false
        case _            => true
    }
    if (newCl == thm.clause.lits) {
      thm
    }
    else new Thm(Clause(newCl), Irreflexive(thm))
  }

  /**  (x = y) ∨ (y = x) ∨ C
    *  ----------------------- removeSym
    *            C
    */
  def removeSym(thm: Thm): Thm = {
    val newCl = thm.clause.distinctBy {
        case (Literal(p1,Eql(x1,y1)),Literal(p2,Eql(x2,y2))) =>
          x1 == y2 && x2 == y1 && p1 == p2
        case _ => false
    }
    if (newCl == thm.clause.lits) {
      thm
    }
    else new Thm(Clause(newCl), RemoveSym(thm))
  }

  private def repeatTopDownConv(
    tm: Term[V,F],
    conv: Term[V,F] => Option[(Term[V,F], Thm)]):
      Option[(Term[V,F], Set[Literal[V,F,P]], Set[Thm])] = {
    val (newTm, topClause, topDeps, topSuccess) =
      util.Fun.repeat ({
        acc:(Term[V,F],Set[Literal[V,F,P]],Set[Thm],Boolean) =>
        val (tm,clause,deps,_) = acc
        conv(tm).map {
          case (newTm, thm) =>
            val eql = Literal[V,F,P](true,Eql(tm,newTm))
            if (thm.clause.contains(eql))
              (newTm, clause | thm.clause - eql, deps + thm,true)
            else throw new Error("Invalid conversion")
        }
      }, (tm,Set[Literal[V,F,P]](),Set[Thm](),false))
    val (newTm2, clause, deps, anySuccess) =
      newTm match {
        case Var(_) => (newTm, topClause, topDeps, false)
        case Fun(f,args) =>
          val (newArgs, newArgsClause, newArgsDeps, anySuccess) =
            args.foldRight((List[Term[V,F]](),topClause,topDeps,false)) {
              case (arg,(restArgs,restClause,restDeps,anySuccess)) =>
                repeatTopDownConv(arg,conv) match {
                  case Some((newArg,argClause,argDeps)) =>
                    (newArg::restArgs,
                      argClause | restClause,
                      argDeps | restDeps, true)
                  case None => (arg::restArgs, restClause, restDeps, anySuccess)
                }
            }
          (Fun(f,newArgs), newArgsClause, newArgsDeps, anySuccess)
      }
    if (anySuccess) {
      repeatTopDownConv(newTm2, conv) match {
        case None => Some(newTm2, clause, deps)
        case fix  => fix.map {
          case (fixTm, fixClause, fixDeps) =>
            (fixTm, clause | fixClause, fixDeps | deps)
        }
      }
    }
    else if (topSuccess)
      Some(newTm2, clause, deps)
    else None
  }

  private def repeatTopDownConvAtom(
    atm: Atom[V,F,P],
    conv: Term[V,F] => Option[(Term[V,F], Thm)]):
      Option[(Atom[V,F,P], Set[Literal[V,F,P]], Set[Thm])] = {
    atm match {
      case Eql(x,y) =>
        for (
          (x2,eqlClause,eqlDeps)   <- repeatTopDownConv(x,conv);
          (y2,eqlClause2,eqlDeps2) <- repeatTopDownConv(y,conv);
          newClause = eqlClause | eqlClause2;
          newDeps   = eqlDeps   | eqlDeps2)
        yield (Eql[V,F,P](x2,y2), newClause, newDeps)
      case Pred(p,args) =>
        args.foldRightM(
          List[Term[V,F]](),
          Set[Literal[V,F,P]](),
          Set[Thm]()) {
          case (arg, (restArgs,accClause,accDeps)) =>
            for ((newArg,newAccClause,newAccDeps) <- repeatTopDownConv(arg,conv))
            yield (
              newArg::restArgs,
              newAccClause | accClause,
              newAccDeps | newAccDeps)
        }.map { case (newArgs,newArgsClause,newArgsDeps) =>
            (Pred(p,newArgs),newArgsClause,newArgsDeps) }
    }
  }

  /**
    *  -------------, P' repeatTopDownConv P conv
    *   ~P v P' v C
    *
    *  where P' is the result of repeatedly traversing P, applying the
    *  conversion conv to every subterm until the conversion fails.
    */
  def repeatTopDownConvRule(
    lit:  Literal[V,F,P],
    conv: Term[V,F] => Option[(Term[V,F], Thm)]) = {
    repeatTopDownConvAtom(lit.atom,conv).map {
      case (newLit, convClause, convDeps) =>
        val newAtom = Literal(lit.isPositive,lit.atom)
        (new Thm(
          Clause(convClause + newAtom.negate + newAtom),
          Conv(convDeps.toList)),
          newAtom)
    }.getOrElse((assume(lit),lit))
  }

  case class UnitThm private[Kernel] (lit: Literal[V,F,P], thm: Thm)

  def substUnit(θ: Subst[V,Term[V,F]], unit: UnitThm) = {
    val newThm = subst(θ, unit.thm)
    UnitThm(unit.thm.clause.head, newThm)
  }

  /** Destruct a clause of exactly only literal. */
  object UnitThm {
    def unapply(thm: Thm): Option[UnitThm] = {
      thm.clause match {
        case UnitClause(lit) => Some(UnitThm(lit,thm))
        case _               => None
      }
    }
  }
}
