package proofpeer.metis

import ClauseInstances._
import LiteralInstances._
import proofpeer.metis.util.Fun._
import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/** A logical kernel for trusted resolution certificates.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
*/
sealed class Kernel[V:Order,F:Order,P:Order] {

  sealed abstract class Inference
  case class Axiom() extends Inference
  case class Assume() extends Inference
  case class Refl() extends Inference
  case class Sym() extends Inference
  case class Trans[V,F,P](xy: Thm, yz: Thm) extends Inference
  case class Equality[V,F,P](
    p: Literal.TermCursor[V,F,P],
    tm: Term[V,F]) extends Inference
  case class RemoveSym[V,F,P](thm: Thm) extends Inference
  case class Conv[V,F,P](literal: Literal[V,F,P], thm: List[Thm]) extends Inference
  case class InfSubst(θ: Subst[V,Term[V,F]], thm: Thm) extends Inference
  case class Resolve[V,F,P](pos: Thm, neg: Thm) extends Inference
  case class Irreflexive[V,F,P](thm: Thm) extends Inference

  case class Thm private[Kernel](clause: Clause[V,F,P], rule: Inference) {
    def isTautology     = clause.isTautology
    def isContradiction = clause.isContradiction

    /**    this
      *  -------- subst θ
      *   this[θ]
      */
    def subst(θ: Subst[V,Term[V,F]]): Thm =
      // For now, removing optimisation where, if the entire clause is unchanged, we
      // do not return a newly constructed clause.
      Thm(Clause(clause.subst(θ)),InfSubst(θ,this))

    // Derived in Hurd. Primitive here
    /**      C ∨ ~(x = x)
      *  ------------------- removeIrrefl
      *           C
      */
    def removeIrrefl: Thm = {
      val newCl = clause.filter {
        case IrreflLit(_) => false
        case _            => true
      }
      if (newCl == clause.lits) {
        this
      }
      else new Thm(Clause(newCl), Irreflexive(this))
    }

    /**  (x = y) ∨ (y = x) ∨ C
      *  ----------------------- removeSym
      *            C
      */
    def removeSym: Thm = {
      val newCl = clause.distinctBy {
          case (Literal(p1,Eql(x1,y1)),Literal(p2,Eql(x2,y2))) =>
            x1 == y2 && x2 == y1 && p1 == p2
          case _ => false
      }
      if (newCl == clause.lits) {
        this
      }
      else new Thm(Clause(newCl), RemoveSym(this))
    }

    /** Cursors to the subterms of the largest literals in a theorem. */
    def largestSubterms(litOrder: LiteralOrdering[V,F,P]) =
      this.clause.largestSubterms(litOrder).map(TermCursor(this,_))
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
    *  ---------------- resolve L, where M is the negation of L.
    *       C ∨ D
    */
  def resolve(lit: Literal[V,F,P], thm1: Thm, thm2: Thm): Option[Thm] = {
    val negLit = lit.negate
    if (thm1.clause.contains(lit) && thm2.clause.contains(negLit)) {
      val cl = (thm1.clause - lit) ++ (thm2.clause - negLit)
      Some(Thm(Clause(cl), Resolve(thm1,thm2)))
    }
    else None
  }

  /**
    *  ------------------ equality L t
    *  ~(s = t) ∨ ~L ∨ L'
    *  Where L' is the result of replacing the subterm s under L with t.
    */
  def equality(tmC: Literal.TermCursor[V,F,P], t: Term[V,F]):
      (Literal[V,F,P], Literal.TermCursor[V,F,P], Thm) = {
    val s      = tmC.get
    val oldLit = tmC.top
    val tmC_   = tmC.replaceWith(t)
    val eq     = Literal(false,Eql[V,F,P](s,t))
    val newLit = tmC_.top
    (oldLit,tmC_,Thm(Clause(Set(eq, oldLit.negate, newLit)),Equality(tmC,t)))
  }

  // Derived rules
  // =============
  import proofpeer.metis.util.RichCollectionInstances._

  /** ---------------------- sym x y
    *    ¬(x = y) ∨ (y = x)
    */
  def sym(x: Term[V,F], y: Term[V,F]) = {
    val xx = refl(x);
    val xxLit = (xx.clause.headOption >>= {
      lit => lit.top.headOption
    }).getOrBug(
      "Refl should produce an equality")
    val (_,_,yx) = equality(xxLit,y)
    resolve(xxLit.top,xx,yx).getOrBug(
      throw new Exception("Sym"))
  }

  type ST[A] = State[Thm,A]
  type TC = Literal.TermCursor[V,F,P]
  def convRule(conv: Term[V,F] => Option[(Term[V,F],Thm)]):
      TC => State[Thm,Option[TC]] =
  tmC => {
    get[Thm] >>= { oldThm =>
      val conved =
        for (
          conved                 <- conv(tmC.get);
          (newTm,eql)            = conved;
          eqLit                  = Literal(true,Eql[V,F,P](tmC.get,newTm));
          (oldLit,newTmC,eqlThm) = equality(tmC,newTm);
          thm                    = resolve(eqLit,eql,eqlThm)
                                       .getOrBug("Invalid conversion");
          thm2                   = resolve(oldLit,oldThm,thm).getOrBug(
                                     "Should be able to resolve on new literal")
          )
        yield newTmC
      conved.point[ST]
    }
  }

  def repeatTermConv(conv: Term[V,F] => Option[(Term[V,F],Thm)]):
      Literal.TermCursor[V,F,P] => State[Thm,TC] = tmC => {
    for (
      newTmC  <- util.Fun.loopM[ST,TC](tmC)(convRule(conv));
      newTmC2 <-
        newTmC.orElse(tmC.down).orElse(tmC.right).orElse(tmC.up) match {
          case None          => tmC.point[ST]
          case Some(newTmC2) => repeatTermConv(conv)(newTmC2)
        })
      yield newTmC2
  }

  /**
    *  -------------, repeatTopDownConv P conv
    *   ~P v P' v C
    *
    *  where P' is the result of repeatedly traversing P, applying the
    *  conversion conv to every subterm until the conversion fails.
    *  Returns None if no conversion took place.
    */
  def repeatTopDownConvRule(
    lit:  Literal[V,F,P],
    conv: Term[V,F] => Option[(Term[V,F], Thm)]) = {
    lit.top.map { tmC =>
      val (thm,newTmC) =  repeatTermConv(conv)(tmC).run(assume(lit))
      (thm,newTmC.top)
    }
  }

  /** Wrap a clause cursor to a subterm. */
  case class TermCursor private[Kernel](
    top: Thm,
    clauseCursor: Clause.TermCursor[V,F,P]) {

    def get = clauseCursor.get
    def literal = clauseCursor.literal
    def subst(θ: Subst[V,Term[V,F]]) = {
      val cursor_ = clauseCursor.subst(θ)
      TermCursor(Thm(cursor_.top,InfSubst(θ,top)),cursor_)
    }
  }
}
