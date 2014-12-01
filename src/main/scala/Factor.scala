package proofpeer.metis

import scala.collection._
import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/** Calculation of clause factor substitutions. */
class Factor[V,F,P](implicit ordV: Order[V]) {
  private abstract sealed class Edge
  private case class FactorEdge(atm1: Atom[V,F,P], atm2: Atom[V,F,P]) extends Edge
  private case class ReflEdge(tm1: Term[V,F], tm2: Term[V,F]) extends Edge

  private abstract sealed class JoinStatus
  private case class Joined() extends JoinStatus
  private case class Joinable(subst: Subst[V,Term[V,F]]) extends JoinStatus
  private case class Apart() extends JoinStatus

  private case class SubEdge(θ: Subst[V,Term[V,F]], edge: Edge)

  private def joinEdge(θ: Subst[V,Term[V,F]], edge: Edge)(
    implicit ordV: Order[V]) = {
    (edge match {
      case FactorEdge(atm1, atm2) => atm1.unify(θ,atm2)
      case ReflEdge(tm1, tm2)     => tm1.unify(θ,tm2)
    }) match {
      case List() => Apart()
      case σ::_   => if (θ == σ) Joined() else Joinable(σ)
    }
  }

  private def factorEdge(lit1: Literal[V,F,P], lit2: Literal[V,F,P]) = {
    if (lit1.isPositive != lit2.isPositive)
      None
    else {
      val edge = FactorEdge(lit1.atom, lit2.atom)
    }
  }

  private def updateApart(θ: Subst[V,Term[V,F]], edges: List[Edge]):
      Option[List[Edge]] = {
    (for (
      edge <- edges;
      if (joinEdge(θ,edge) match {
        case Joined()    => return None
        case Joinable(_) => true
        case Apart()     => false
      }))
      yield edge).some
  }

  private def initEdges(
    apart: List[Edge],
    subEdges: List[SubEdge]) = {
    var apart = List[Edge]()
    val allEdges = (subEdges.map { case SubEdge(_,edge) => edge }).tails.drop(2)
    for (
      acc@(SubEdge(θ, edge),agenda) <- subEdges.iterator zip allEdges;
      if (updateApart(θ,apart) match {
        case Some(apart2) => apart = edge::apart2; true
        case None         => false
      }))
      yield (apart,θ,agenda)
  }

  private def combineSym[A,B](xs: Seq[A])(f: (A,A) => B): Seq[B] = {
    xs.headOption match {
      case None    => List()
      case Some(x) =>
        val ys = xs.tail
        (ys map (f(x,_))) ++ combineSym(ys)(f)
    }
  }

  private def mkEdges(lits: List[Literal[V,F,P]]) = {
    val litsV   = lits.view
    val eqs     = litsV.map { _ match {
      case lit@Literal(true,Eql(x,y)) => Some(lit)
      case _                          => None
    }}.flatten
    val neqs    = litsV.map { _ match {
      case lit@Literal(false,Eql(x,y)) => Some(lit)
      case _                           => None
    }}.flatten
    val apart = (List() ++ eqs.map { _ match {
      case Literal(true,Eql(x,y)) =>
        x.unify(Subst.empty,y).map { _ => ReflEdge(x,y) }
    }}).flatten[Edge]
    val neqSubEdges = neqs.map { _ match {
      case Literal(false,Eql(x,y)) =>
        x.unify(Subst.empty,y).map { θ => SubEdge(θ,ReflEdge(x,y)) }
    }}.flatten[SubEdge]
    val allLits = lits.map {
      case lit@Literal(p,Eql(x,y)) => List(lit,Literal(p,Eql[V,F,P](y,x)))
      case lit                     => List(lit)
    }.flatten
    val litSubEdges = combineSym(allLits) {
      case (Literal(pol,atm1),Literal(pol2,atm2)) if pol == pol2 =>
        atm1.unify(Subst.empty,atm2).map {
          case θ => SubEdge(θ,FactorEdge(atm1,atm2))
        }
      case _ => List()
    }.flatten[SubEdge]
    initEdges(apart,List() ++ neqSubEdges ++ litSubEdges)
  }

  private type Factor              = (List[Edge], Subst[V,Term[V,F]])
  private type StateFactor[M[_],A] = StateT[M, Factor, A]
  private type Factoring[A]        = StateFactor[List,A]

  private def put(apart:List[Edge], θ:Subst[V,Term[V,F]]) =
    State.put((apart,θ)).lift[List]

  private def fact1(edge:Edge): Factoring[Unit] = {
    State.get.lift[List].flatMap {
      case (apart,θ) =>
        joinEdge(θ,edge) match {
          case Joinable(σ) =>
            (List(put(apart,σ)) ++
              (updateApart(σ,apart) match {
                case Some(apart2) => List(put(apart2,σ))
                case _            => List()
              })).liftM[StateFactor].join
          case _            => ().point[Factoring]
        }
    }
  }

  private def fact(apart: List[Edge], θ: Subst[V,Term[V,F]], agenda: List[Edge]) = {
    agenda.map(fact1).sequence.exec(apart,θ).map(_._2)
  }

  /** Obtain the factorings substitutions of clause cl. Each factoring
      substitution θ is such that cl[θ], after removal of instances of
      reflexivity and removing redundant symmetric equalities, contains
      fewer literals than cl. Moreover, for any such substitution σ,
      cl[σ] is subsumed by cl[θ] for one of the returned substitutions θ.
      */
  def factor(cl: Clause[V,F,P]): Iterator[Subst[V,Term[V,F]]] = {
    mkEdges(List() ++ cl.lits).flatMap {
      case (apart,θ,agenda) => fact(apart,θ,agenda)
    }
  }
}
