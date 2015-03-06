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
  case class Equality[V,F,P](p: Vector[Int], lit: Literal[V,F,P]) extends Inference
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
      this.clause.largestSubterms(litOrder).map(new TermCursor(this,_))
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
  def equality(lit: Literal.TermCursor[V,F,P], t: Term[V,F]) = {
    val s  = lit.get
    val eq = Literal(false,Eql[V,F,P](s,t))
    Thm(Clause(Set(eq, lit.top.negate, lit.replaceWith(t))),
      Equality(lit.path,lit.top))
  }

  // Derived rules
  // =============

  /** ---------------------- sym x y
    *    ¬(x = y) ∨ (y = x)
    */
  def sym(x: Term[V,F], y: Term[V,F]) = {
    val xx = refl(x);
    val xxLit = (xx.clause.headOption >>= (_.lhs)).getOrElse(
      throw new Exception("Refl should produce an equality"))
    val yx = equality(xxLit,y)
    val foo =
      resolve(xxLit.top,xx,yx).getOrElse(
        throw new Exception("Sym"))
    Debug.debugShowsClause(foo.clause)
    foo
  }

  // Write out a set of dependencies and a possible final clause containing
  // the final equality and any additional hypotheses.
  type W[A] = Writer[(Set[Thm],Option[Set[Literal[V,F,P]]]),A]

  def convRule(conv: Term[V,F] => Option[(Term[V,F],Thm)]):
      Term[V,F] => W[Option[Term[V,F]]] = tm => {
  import proofpeer.metis.util.RichCollectionInstances._
  conv(tm) match {
    case None => none.point[W]
    case Some((newTm,thm)) =>
      // TODO: We can be more general than this, but since METIS only does
      // conversions with unit equalities, we'll regard it as a bug if a non-unit
      // equality is used.
      val eql = Literal[V,F,P](true,Eql(tm,newTm))
      thm.clause.lits.singleton match {
        case Some(lit) if lit == eql =>
          newTm.some.point[W] :++> ((Set(thm),some(thm.clause - eql)))
        case _ =>
          throw new IllegalArgumentException("Invalid conversion")
      }
  }
  }

  private def tryRepeatTopDownConv(
    tm: Term[V,F],
    conv: (Term[V,F] => W[Option[Term[V,F]]])): W[Option[Term[V,F]]] = {
    val topConv = loopM1[W,Term[V,F]](tm)(conv)
    val depthConv: W[Option[Term[V,F]]] = topConv >>= { rewriteTop =>
      val newTopTm = rewriteTop.getOrElse(tm)
      newTopTm match {
        case Fun(f,args) =>
          args.traverse(tryRepeatTopDownConv(_,conv)) >>= { newArgs =>
            val changed = newArgs.exists(_.isDefined)
            if (!changed)
              none.point[W]
            else
              repeatTopDownConv(
                Fun(f,(newArgs,args).zipped.map { _.getOrElse(_) }),
                conv).map(_.some)
          }
        case _ => none.point[W]
      }
    }
    (depthConv |@| topConv) { _.orElse(_) }
  }

  private def repeatTopDownConv(
    tm: Term[V,F],
    conv: (Term[V,F] => W[Option[Term[V,F]]])): W[Term[V,F]] =
    tryRepeatTopDownConv(tm, conv).map { _.getOrElse(tm) }

  private def repeatTopDownConvAtom(
    atm: Atom[V,F,P],
    conv: Term[V,F] => W[Option[Term[V,F]]]): W[Atom[V,F,P]] = {
    atm match {
      case Eql(x,y) =>
        (repeatTopDownConv(x,conv) |@| repeatTopDownConv(y,conv)) { Eql(_,_) }
      case Pred(p,args) =>
        args.traverse(repeatTopDownConv(_,conv)).map { Pred(p,_) }
    }
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

    val isPositive = lit.isPositive
    val atom       = lit.atom
    val ((deps,lits),newAtom) = repeatTopDownConvAtom(atom,convRule(conv)).run

    lits.map { lits =>
      (Thm(
        Clause(lits + Literal(!isPositive,atom) + Literal(isPositive,newAtom)),
        Conv(lit, deps.toList)),
        Literal(lit.isPositive,newAtom))
    }
  }

  /** Wrap a clause cursor to a subterm. */
  case class TermCursor private[Kernel](
    top: Thm,
    clauseCursor: Clause.TermCursor[V,F,P]) {

    def get = clauseCursor.get
    def literal = clauseCursor.literal
    def substTop(θ: Subst[V,Term[V,F]]) = {
      val cursor_ = clauseCursor.substTop(θ)
      new TermCursor(Thm(cursor_.top,InfSubst(θ,top)),cursor_)
    }
  }
}
