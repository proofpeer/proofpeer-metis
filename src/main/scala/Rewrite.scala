package proofpeer.metis

import scala.collection.immutable._
import scalaz._
import Scalaz._

/** A rewriting system. */
case class Rewriting[V:Order,F:Order,P,K <: Kernel[V,F,P]](kernel: K)(
  implicit ordFun: Order[Fun[V,F]]) {

  import ClauseInstances._

  val kbo = KnuthBendix.kbo[V,F]

  abstract sealed class Direction
  case class LeftToRight() extends Direction
  case class RightToLeft() extends Direction

  abstract sealed class Orientation
  case class Directed(dir:Direction) extends Orientation
  case class Bidirectional() extends Orientation

  case class Equation(
    l: Term[V,F],
    r: Term[V,F],
    eq:kernel.Thm,
    orientation: Orientation)

  abstract sealed class EqSide
  case class LHS() extends EqSide
  case class RHS() extends EqSide

  case class ReduceAcc(
    redexesAcc:  Set[Int],
    subtermsAcc: Set[Int],
    todoAcc:     Set[Int],
    changedAcc:  Set[Int])

  class Rewrite private (
    known:    Map[Int,Equation],
    redexes:  Nets.TermNet[F,(Int,Direction)],
    subterms: Nets.TermNet[F,(Int,EqSide,Term.Path)],
    waiting:  Set[Int]) {

    def this() =
      this(
        Map[Int,Equation](),
        new Nets.TermNet[F,(Int,Direction)],
        new Nets.TermNet[F,(Int,EqSide,Term.Path)],
        TreeSet[Int]())

    /*   x = y ∨ C
     *  ------------ symRule x y
     *   y = x v C
     */
    private def symRule(x: Term[V,F], y: Term[V,F], thm: kernel.Thm) = {
      val sym = kernel.sym(x,y)
      kernel.resolve(Literal(true,Eql(x,y)),thm,sym).get
    }

    def orient(l: Term[V,F], r: Term[V,F]) = {
      kbo.tryCompare(l,r) match {
        case Some(Ordering.LT) => Some(Directed(LeftToRight()))
        case Some(Ordering.GT) => Some(Directed(RightToLeft()))
        case Some(Ordering.EQ) => None
        case None              => Some(Bidirectional())
      }
    }

    def addRedexes(id: Int, orientedEq: Equation) = {
      orientedEq.orientation match {
        case Directed(LeftToRight()) =>
          redexes.insert(orientedEq.l,(id,LeftToRight()))
        case Directed(RightToLeft()) =>
          redexes.insert(orientedEq.r,(id,RightToLeft()))
        case Bidirectional() =>
          redexes.insert(orientedEq.l,(id,LeftToRight()))
            .insert(orientedEq.r,(id,RightToLeft()))
      }
    }
    def add(id: Int, eqn:Equation) = {
      if (known.contains(id))
        this
      else {
        orient(eqn.l,eqn.r) match {
          case None => this
          case Some(ort) =>
            val known = this.known + (id → Equation(eqn.l,eqn.r,eqn.eq,ort))
            val redexes = addRedexes(id,eqn)
            val waiting = this.waiting + id
            new Rewrite(known,redexes,this.subterms,waiting)
        }
      }
    }

    type Conv = Term[V,F] => Option[(Term[V,F], kernel.Thm)]

    // TODO: We're skipping the wellOriented check. I cannot see how this check can
    // deliver anything other than true, given that additions to redexes and the
    // known set are always made at the same time with the same id and orientation.
    private def rewr(id: Int): Conv =
      tm => {
        val thms = redexes.matches(tm).view.map {
          case (id2,ort) =>
            for (eqn <- this.known.get(id2)
              if (id != id2);
              (l,r) = ort match {
                case LeftToRight() => (eqn.l,eqn.r)
                case RightToLeft() => (eqn.r,eqn.l)
              };
              θ <- l.patMatch(Subst.empty,tm).headOption;
              // Not bothering to "normalise" (remove identity substitutions)
              tm2 = tm.subst(θ);
              if (eqn.orientation == Bidirectional()
                || kbo.tryCompare(tm,tm2) == Some(Ordering.GT));
              thm = ort match {
                case LeftToRight() =>
                  kernel.subst(θ,eqn.eq)
                case RightToLeft() =>
                  kernel.subst(θ,symRule(eqn.l,eqn.r,eqn.eq))
              })
              yield (tm2,kernel.subst(θ,thm))
        }
        thms.find(_.isDefined).flatten
      }

    type NeqConvMap = Map[Literal[V,F,P],Conv]
    // Send kbo-comparable literals to a conversion which sends the larger to the smaller.
    private def mkNeqConv(l: Term[V,F], r: Term[V,F]) = {
      kbo.tryCompare(l,r).flatMap {
        case Ordering.GT =>
          Some { term:Term[V,F] =>
            if (term == l)
              Some(r,kernel.assume(Literal(true,Eql(l,r))))
            else
              None
          }
        case Ordering.LT =>
          Some { term:Term[V,F] =>
            if (term == r)
              Some(r,kernel.sym(l,r))
            else None
          }
        case Ordering.EQ => None
      }
    }

    private def mkNeqConvMap(lits: Set[Literal[V,F,P]]) = {
      lits.foldLeft((Map[Literal[V,F,P],Conv](),Set[Literal[V,F,P]]())) {
        case ((mapAcc,litsAcc),lit@NeqLit(l,r)) => mkNeqConv(l,r) match {
          case Some(conv) => (mapAcc + (lit → conv), litsAcc)
          case None       => (mapAcc, litsAcc + lit)
        }
        case ((mapAcc,litsAcc),lit) => (mapAcc, litsAcc + lit)
      }
    }

    // Apply the first successful conversion in the map
    private def neqConvToConv(map: NeqConvMap) : Conv =
      tm => map.collectFirst { case (_,conv) => conv(tm) }.flatten

    private def mkNeqRule(id: Int, map: NeqConvMap)(lit: Literal[V,F,P]) = {
      val neqConv    = Function.unlift(neqConvToConv(map))
      val rewrConv   = Function.unlift(rewr(id))
      val conv       = neqConv.orElse(rewrConv).lift
      kernel.repeatTopDownConvRule(lit,conv)
    }

    // Given a clause, interreduce all negated (conditional) equalities with the
    // main rewrites, and use the final interreduced set of rewrites against all
    // literals in the clause.
    private def interRewriteNeqs(thm: kernel.Thm, id: Int) = {

      val (map,lits) = mkNeqConvMap(thm.clause)

      def interRewrite1(
        acc: (NeqConvMap, Set[Literal[V,F,P]], kernel.Thm, Boolean),
        kv: (Literal[V,F,P], Conv)) = {
        val (key,_)    = kv
        val (map, lits, thm, changed) = acc
        val (litConvThm,newLit) = mkNeqRule(id,map)(key)
        if (key != newLit) {
          val newThm = kernel.resolve(newLit,litConvThm,thm).get
          newLit match {
            case NeqLit(l,r) =>
            mkNeqConv(l,r) match {
              case Some(conv) =>
                (map - key + (newLit → conv), lits, newThm, true)
              case None       => (map - key, lits + key, thm, changed)
            }
            case _ => (map - key, lits + key, thm, changed)
          }
        }
        else acc
      }

      def interRewrite(
        map: NeqConvMap,
        lits: Set[Literal[V,F,P]],
        thm: kernel.Thm):
          (NeqConvMap, Set[Literal[V,F,P]], kernel.Thm) = {
        val (newMap, newLits, newThm, changed) =
          map.foldLeft((map, lits, thm, false))(interRewrite1)
        if (changed)
          interRewrite(newMap, newLits, newThm)
        else (map,lits,newThm)
      }

      val (newMap, newLits, newThm) = interRewrite(map, lits, thm)

      val neqRule = mkNeqRule(id,newMap)_

      val finalThm = lits.foldLeft(newThm) {
        case (accThm,lit) =>
          if (newThm.clause.contains(lit))
            kernel.resolve(lit,neqRule(lit)._1,accThm).get
          else accThm
      }

      (finalThm,id)
    }

    private def rewriteEqn(thmId: Int, eqn: Equation) = {
      val (map, _)         = mkNeqConvMap(eqn.eq.clause)
      val eqLit            = Literal(true,Eql[V,F,P](eqn.l,eqn.r))
      val strongEqn        = !eqn.eq.clause.contains(eqLit)
      val (eqThm,newEqLit) = mkNeqRule(thmId,map)(eqLit)
      // Note: Leaving out optimisation checking whether the literal has changed
      newEqLit match {
        case newEqLit@Literal(true,Eql(l,r)) =>
          val thm =
            if (strongEqn)
              eqn.eq
            else if (eqThm.clause.contains(eqLit.negate))
              kernel.resolve(eqLit, eqn.eq, eqThm).get
            else eqThm
          (thm,l,r)
      }
    }

    private def isOriented(eqn: Equation) = {
      eqn.orientation match {
        case Bidirectional() => true
        case _               => false
      }
    }

    private def pick(todo: Set[Int]) = {
      todo.collectFirst(Function.unlift { id =>
        known.get(id).filter {!isOriented(_)}
        .map((id,_))
      }).orElse(
        todo.collectFirst(Function.unlift { id => known.get(id).map ((id,_)) }))
    }

    private def rebuild(acc: ReduceAcc) = {
      val newRedexes  = this.redexes.filter {
        case (thmId, _) => !acc.redexesAcc.contains(thmId)
      }
      val newSubterms = this.subterms.filter {
        case (thmId,_,_) => !acc.redexesAcc.contains(thmId)
      }

      val newRedexes2 = acc.redexesAcc.foldLeft(newRedexes) {
        case (theRedexes, id) =>
          this.known.get(id) match {
            case None      => theRedexes
            case Some(eqn) => addRedexes(id,eqn)
          }
      }
      val newSubterms2 = acc.subtermsAcc.foldLeft(newSubterms) {
        case (theSubterms, id) =>
          this.known.get(id) match {
            case None      => theSubterms
            case Some(eqn) => addSubterms(id,eqn)
          }
      }
      new Rewrite(known,newRedexes2,newSubterms2,waiting)
    }

    private def findReducibles(
      todo: Set[Int],
      thmId: Int,
      l:Term[V,F],
      r:Term[V,F],
      orientation: Orientation) = {
      val resides =
        orientation match {
          case Directed(LeftToRight()) => List((l,r,true))
          case Directed(RightToLeft()) => List((r,l,true))
          case _                       => List((l,r,false),(r,l,false))
        }
      resides.foldLeft(todo) {
        case (todo,(l,r,isOriented)) =>
          subterms.matched(l).foldLeft(todo) {
            case (todo,(thmId2,eqSide,path))
                if thmId != thmId2
                && !(todo.contains(thmId2))
                && isOriented =>

              // Grab the actual (non-quotiented) subterm in the theorem with thmId2
              val eqn = known.get(thmId2).get
              val subterm = eqSide match {
                case LHS() => eqn.l.subtermAt(path)
                case RHS() => eqn.r.subtermAt(path)
              }

              // Does this subterm really match l and does l → r reduce the term order?
              val doesRewrite =
                l.patMatch(Subst.empty,subterm).headOption match {
                  case None    => false
                  case Some(θ) =>
                    // skip normalisation
                    kbo.tryCompare(l,r.subst(θ)) match {
                      case Some(Ordering.GT) => true
                      case _                 => false
                    }
                }

              if (doesRewrite)
                todo + thmId2
              else
                todo

            case _ => todo
          }
      }
    }

    private def addSubterms(thmId: Int, eqn: Equation) = {
      val newSubterms = eqn.l.allSubterms.foldLeft(this.subterms) {
        case (subterms,(path,subterm)) =>
          subterms.insert(subterm,(thmId,LHS(),path))
      }
      eqn.r.allSubterms.foldLeft(newSubterms) {
        case (subterms,(path,subterm)) =>
          subterms.insert(subterm,(thmId,RHS(),path))
      }
    }

    private def reduce1(isNew: Boolean, thmId: Int, eqn: Equation, acc: ReduceAcc):
        (Rewrite,ReduceAcc) = {
      val (newEqThm,l,r) = rewriteEqn(thmId, eqn)
      val isIdentical   = l == eqn.l && r == eqn.r

      // We only need to check the original orientation. If the
      // equation has switched orientation, then we know that the
      // redexes are different, since we always rewrite to the smaller
      // term.
      val sameRedexes = eqn.orientation match {
        case Directed(LeftToRight()) => eqn.l == l
        case Directed(RightToLeft()) => eqn.r == r
        case _                       => isIdentical
      }

      val redexesAcc =
        if (sameRedexes)
          acc.redexesAcc
        else acc.redexesAcc + thmId
      val subtermsAcc =
        if (isNew || isIdentical)
          acc.subtermsAcc
        else
          acc.subtermsAcc + thmId
      val changedAcc =
        if (!isNew && isIdentical)
          acc.changedAcc
        else acc.changedAcc + thmId
      val newOrientation =
        if (sameRedexes) {
          Some(eqn.orientation)
        }
        else orient(l,r)
      newOrientation match {
        case None =>
          (new Rewrite(this.known - thmId, this.redexes, this.subterms, this.waiting),
            ReduceAcc(redexesAcc, acc.subtermsAcc, acc.todoAcc, changedAcc))
        case Some(orientation) =>
          val todoAcc =
            if (!isNew && sameRedexes)
              acc.todoAcc
            else findReducibles(acc.todoAcc,thmId,l,r,orientation)
          val newKnown =
            if (isIdentical)
              this.known
            else known + (thmId → Equation(l,r,newEqThm,orientation))
          val newRedexes =
            if (sameRedexes)
              this.redexes
            else addRedexes(thmId, eqn)
          val newSubterms =
            if (!isNew && isIdentical)
              this.subterms
            else addSubterms(thmId, eqn)
          (new Rewrite(newKnown, newRedexes, newSubterms, this.waiting),
            ReduceAcc(redexesAcc, subtermsAcc, todoAcc, changedAcc))
      }
    }

    private def reduceAcc(acc: ReduceAcc): (Rewrite, Set[Int]) = {
      pick(acc.todoAcc) match {
        case Some((id,eqn)) =>
          val (newRw, newAcc) = reduce1(false, id, eqn, ReduceAcc(
            acc.redexesAcc, acc.subtermsAcc, acc.todoAcc - id, acc.changedAcc))
          newRw.reduceAcc(newAcc)
        case None =>
          pick(this.waiting) match {
            case Some((id,eqn)) =>
              val newRw = new Rewrite(known, redexes, subterms, this.waiting - id)
              val (newRw2, newAcc) =
                newRw.reduce1(true, id, eqn, acc)
              newRw2.reduceAcc(newAcc)
            case None => (rebuild(acc),acc.changedAcc)
          }
      }
    }

    def reduce = reduceAcc(ReduceAcc(Set(),Set(),Set(),Set()))
  }
}
