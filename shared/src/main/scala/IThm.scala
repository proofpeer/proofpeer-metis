package proofpeer.metis

import ClauseInstances._
import proofpeer.metis.util._
import RichCollectionInstances._
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/** Factory for theorems with identifiers, preserved by normalising rules.
  *
  * @tparam V  The alphabet from which variable names are drawn
  * @tparam F  The alphabet from which functor names are drawn
  * @tparam P  The alphabet from which predicate names are drawn
  * @tparam FV The state type used to generate fresh variables.
  * @param kernel    the logic defining the thm type
  * @param nextV     state-based generator of fresh variables
  * @param initState initial state for the fresh variable generator
  */
case class IThmFactory[V:Order,F:Order,P:Order,FV,K<:Kernel[V,F,P]](
  kernel: K,
  initState: FV,
  nextV : (FV,V) => (FV,V),
  litOrder: LiteralOrdering[V,F,P],
  factorClause: Clause[V,F,P] => Iterator[Subst[V,Term[V,F]]])(implicit
    termOrd: PartialOrder[Term[V,F]]) {

  var theState = initState

  /** Theorems with identifiers. All instance methods return new theorems but
      preserve the id. */
  case class IThm private[IThmFactory] (id: Int, thm: kernel.Thm) {
    def clause = thm.clause
    def rule   = thm.rule
    def isTautology     = thm.isTautology
    def isContradiction = thm.isContradiction

    // No support for typed vars as yet (do we care about them for pp?)
    /* ~(v = t) ∨ C
     *  ------------ expandAbbrevs
     *     C[t/v]
     */
    private def expandAbbrevs(thm: kernel.Thm) = {
      val firstSubst =
        thm.clause.toIterator.map {
          case NeqLit(l,r) if l != r => l.unify(Subst.empty,r).headOption
          case _                     => None
        }.find (_.isDefined).flatten
      firstSubst.map(thm.subst(_))
        .getOrElse(thm.removeIrrefl)
    }

    /** Simplify: chuck out if its a tautology. Otherwise expand all abbreviations
      * until a fixpoint is reached.
      */
    def simplify = {
      def simp(thm: kernel.Thm): Option[kernel.Thm] = {
        if (thm.clause.isTautology) {
          None
        }
        else {
          val simpedThm = expandAbbrevs(thm.removeSym)
          if (thm == simpedThm) {
            Some(simpedThm)
          }
          else simp(simpedThm)
        }
      }
      simp(this.thm).map { new IThm(this.id,_) }
    }

    /** Largest rewrites in a theorem. */
    def rewrites = RewriteCursor.rewrites(this)

    /**  L ∨ C       M
      *  -------------- resolveUnit L, where M is matched to the negation of L.
      *         C
      */
    def resolveUnit(lit: Literal[V,F,P], iunit: UnitIThm) = {
      for (
        θ       <- iunit.lit.patMatch(Subst.empty,lit.negate).headOption;
        unitThm = iunit.ithm.thm.subst(θ);
        unitLit = unitThm.clause.lits.toList match {
          case List(unitLit_) => unitLit_
          case _              => throw new Error(
            "Bug: Substitution should preserve unitness.")
        };
        resolvent <- kernel.resolve(unitLit.negate, this.thm, unitThm))
      yield new IThm(this.id, resolvent)
    }

    /** Replace all variables in a theorem with fresh variables. */
    def freshen = {
      val fvs = this.clause.flatMap(_.frees)
      val θ   = fvs.foldLeft(Subst.empty[V,Term[V,F]]) {
        case (θ,v) =>
          val (newState,freshVar) = nextV(theState,v)
          theState = newState
          θ.bind(v,Var(freshVar)).get
      }
      new IThm(this.id,this.thm.subst(θ))
    }

    /** Apply a conversion to all lits of a theorem. */
    def repeatTopDownConvRule(conv: Term[V,F] => Option[(Term[V,F], kernel.Thm)]) = {

      val lits     = clause.lits.toList
      val convThms = lits.map { kernel.repeatTopDownConvRule(_,conv) }
      val changed  = convThms.exists(_.isDefined)
      if (!changed)
        None
      else {
        val convThmsLits = (convThms,lits).zipped.map { case (convThm,lit) =>
          convThm.map { (_,lit) }
        }
        val newThm = convThmsLits.flatten.foldLeft(thm)
        { case (thm,((convThm,_),lit)) =>
          kernel.resolve(lit,thm,convThm).
            getOrBug("Conversions should never fail.")
        }
        IThm(this.id,newThm).some
      }
    }
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
    *  ------------------ resolve L M, where L unifies with the negation of M
    *       C ∨ D
    *
    * After unification, both literals should be the largest in their clauses.
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
      thm1_ = thm1.subst(θ);
      thm2_ = thm2.subst(θ);
      if litOrder.isMaximal(thm1_.clause)(lit1_);
      if litOrder.isMaximal(thm2_.clause)(nlit2_);
      resolvent <- kernel.resolve(lit1_,thm1_,thm2_))
    yield new IThm(newId, resolvent)
  }

  /** Obtain all factorings of a theorem's largest literals, generating new clause
      ids for each. */
  def factor(ithm: IThm): List[IThm] = {
    for (
      θ <- factorClause(Clause(ithm.clause.largestLiterals(litOrder))).toList)
    yield new IThm(ithm.id, ithm.thm.subst(θ))
  }

  case class UnitIThm private[IThmFactory](lit:Literal[V,F,P], ithm: IThm)

  /** Destruct a clause of exactly one literal. */
  object UnitIThm {
    def getUnit(ithm: IThm): Option[UnitIThm] =
      ithm.clause.lits.singleton.map { UnitIThm(_,ithm) }
  }

  /** A cursor to a rewrite in an IThm */
  case class RewriteCursor private[IThmFactory](
    top: kernel.Thm,
    eql: Eql[V,F,P],
    direction: Direction) {

    /** The theorem with equation appearing as lhs = rhs */
    def topRewrite = direction match {
      case LeftToRight() => top
      case RightToLeft() =>
        kernel.resolve(literal,top,kernel.sym(eql.l,eql.r)).getOrBug("topRewrite")
    }

    kernel.sym(lhs,rhs)

    def lhs = lr._1
    def rhs = lr._2
    def lr = direction match {
      case LeftToRight() => (eql.l,eql.r)
      case RightToLeft() => (eql.r,eql.l)
    }
    def substTop(θ: Subst[V,Term[V,F]]) =
      new RewriteCursor(top.subst(θ),eql.subst(θ),direction)
    def literal = Literal(true,eql)
    def literalRewrite = {
      val (l,r) = lr
      Literal(true,Eql[V,F,P](l,r))
    }
  }

  object RewriteCursor {
    def rewrites(ithm: IThm): Set[RewriteCursor] =
      for (
        (lit@Literal(true,eql@Eql(x,y))) <- ithm.clause.lits;
        if litOrder.isMaximal(ithm.clause.lits)(lit);
        ort <-
        (if (termOrd.tryCompare(x,y) === Some(Ordering.GT))
          List(LeftToRight()) else List()) ++
        (if (termOrd.tryCompare(y,x) === Some(Ordering.GT))
          List(RightToLeft()) else List()))
      yield
        new RewriteCursor(ithm.thm,eql,ort)
  }

  abstract sealed class Direction
  case class LeftToRight() extends Direction
  case class RightToLeft() extends Direction

  def paramodulate(rewrite: RewriteCursor, subterm: kernel.TermCursor) = {
    val lhs = rewrite.lhs
    for (
      θ     <- lhs.unify(Subst.empty,subterm.get).headOption;
      lhs_  = lhs.subst(θ);
      rewr_ = rewrite.substTop(θ);
      lit_  = subterm.substTop(θ);
      if litOrder.isMaximal(rewr_.top.clause.lits)(rewr_.literal);
      if litOrder.isMaximal(subterm.top.clause.lits)(subterm.literal);
      Ordering.GT <- termOrd.tryCompare(rewr_.lhs,rewr_.rhs);
      equal = kernel.equality(lit_.clauseCursor.literalCursor, rewr_.rhs);
      val resolvent = (for (
        resolvent1 <- kernel.resolve(rewr_.literalRewrite,rewr_.topRewrite,equal);
        resolvent2 <- if (resolvent1.clause(lit_.literal.negate))
                        kernel.resolve(lit_.literal,lit_.top,resolvent1)
                      else Some(resolvent1))
      yield resolvent2).getOrBug("paramodulate"))
    yield IThm(newId, resolvent)
  }
}
