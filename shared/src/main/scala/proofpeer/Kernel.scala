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

  sealed abstract class Inference {
    def depth: Int =
      1 + (this match {
        case RemoveSym(thm)       => thm.rule.depth
        case Irreflexive(thm)     => thm.rule.depth
        case InfSubst(_,thm)      => thm.rule.depth
        case Resolve(_,thm1,thm2) => thm1.rule.depth.max(thm2.rule.depth)
        case _ => 0
      })
    def size: Int =
      1 + (this match {
        case RemoveSym(thm)       => thm.rule.size
        case Irreflexive(thm)     => thm.rule.size
        case InfSubst(_,thm)      => thm.rule.size
        case Resolve(_,thm1,thm2) => thm1.rule.depth + thm2.rule.size
        case _ => 0
      })
  }
  case class Axiom() extends Inference
  case class Assume() extends Inference
  case class Refl() extends Inference
  case class Equality(
    p: Literal.TermCursor[V,F,P],
    tm: Term[V,F]) extends Inference
  case class RemoveSym(thm: Thm) extends Inference
  case class Irreflexive(thm: Thm) extends Inference
  case class InfSubst(θ: Subst[V,Term[V,F]], thm: Thm) extends Inference
  case class Resolve(atom: Atom[V,F,P], pos: Thm, neg: Thm) extends Inference

  case class Thm private[Kernel](clause: Clause[V,F,P], rule: Inference) {
    def isTautology     = clause.isTautology
    def isContradiction = clause.isContradiction

    /**    this
      *  -------- subst θ
      *   this[θ]
      */
    def subst(θ: Subst[V,Term[V,F]]): Thm = {
      val newCl = clause.subst(θ)
      if (newCl == clause)
        this
      else Thm(newCl,InfSubst(θ,this))
    }

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
      Some(Thm(Clause(cl),
        if (lit.isPositive) Resolve(lit.atom,thm1,thm2)
        else Resolve(negLit.atom,thm2,thm1)))
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
    val neq    = Literal(false,Eql[V,F,P](s,t))
    val newLit = tmC_.top
    (oldLit,tmC_,Thm(Clause(Set(neq, oldLit.negate, newLit)),Equality(tmC,t)))
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
      lit => lit.topLeft.headOption
    }).getOrBug(
      "Refl should produce an equality")
    val (_,_,yx) = equality(xxLit,y)
    resolve(xxLit.top,xx,yx).getOrBug("Sym")
  }

  type ST[A] = State[Thm,A]
  type M[A]  = OptionT[ST,A]
  import OptionT._
  import syntax._
  implicit class toTryM[A](x: M[A]) {
    // Try to perform a computation, keeping the original value on failure.
    def andMaybe(f: A => M[A]): M[A] =
      x >>= (y => f(y).orElse(y.point[M]))

    def getSuccess: M[Option[A]] =
      State[Thm,Option[A]](s => x.run(s)).liftM

    def orElse(y: M[A]) =
      State[Thm,Option[A]](s => {
        val (sx,x2) = x.run(s)
        x2 match {
          case None => y.run(sx)
          case _    => (sx,x2)
        }
      })
  }

  implicit class toOptM[A](x: Option[A]) {
    def liftOpt: M[A] = {
      x.map(_.point[M]).getOrElse(none)
    }
  }

  type TC = Literal.TermCursor[V,F,P]
  def convRule(conv: Term[V,F] => Option[(Term[V,F],Thm)]): TC => M[TC] =
    tmC => {
      for (
        oldThm <- get[Thm].liftM;
        conved <- conv(tmC.get).liftOpt;
        (newTm,eql) = conved;
        if tmC.get != newTm;
        eqLit                  = Literal(true,Eql[V,F,P](tmC.get,newTm));
        (oldLit,newTmC,eqlThm) = equality(tmC,newTm);
        thm                    = resolve(eqLit,eql,eqlThm).getOrBug(
          "Invalid conversion");
        // In case we try to convert the lhs of x = y with that same equation.
        thm_                   = if (oldLit == eqLit) eqlThm else thm;
        newThm                 = resolve(oldLit,oldThm,thm_).getOrBug(
          "Should be able to resolve on new literal");
        ()   <- put[Thm](newThm).liftM)
      yield newTmC
    }

  // Exhaustively convert this term and all its descendents.
  def termConv(conv: Term[V,F] => Option[(Term[V,F],Thm)]):
      Literal.TermCursor[V,F,P] => M[TC] = tmC => {
    for (
      // Go down and try to convert
      downTmC <- (tmC.down.liftOpt >>= (termConv(conv)(_))).getSuccess;
      // Go back up
      nextTry = downTmC.map(_.up.getOrBug(
        "Moved down. Must be able to move back up.")) getOrElse tmC;
      // Convert this term and maybe loop. If that fails, convert right.
      nextTmC <- ((convRule(conv)(nextTry) andMaybe (termConv(conv(_))))
        orElse (nextTry.right.liftOpt >>= (termConv(conv)(_))))
           .getSuccess;
      // Conversion is successful if either of the previous steps were.
      nextTmC2 <- (nextTmC orElse downTmC).liftOpt
    )
    yield nextTmC2
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
    for (
      tmC          <- lit.topLeft;
      (thm,newTmC)  = termConv(conv)(tmC).run(assume(lit));
      newLit       <- newTmC.map(_.top))
    yield (thm,newLit)
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

  case class UnitThm(lit:Literal[V,F,P], thm: Thm)

  /** Destruct a clause of exactly one literal. */
  object UnitThm {
    def getUnit(thm: Thm): Option[UnitThm] =
      thm.clause.lits.singleton.map { UnitThm(_,thm) }
  }
}
