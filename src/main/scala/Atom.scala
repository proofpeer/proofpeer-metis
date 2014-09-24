package proofpeer.metis.atom

import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import scalaz.std._

abstract sealed class Atom[V,F]
case class Var[V,F](v: V) extends Atom[V,F]
case class Comb[V,F](f: F, args: List[Atom[V,F]]) extends Atom[V,F]

class BatOrder[T1](ord1: Order[T1]) {
  def lexico[T2](ord2: Order[T2]) =
    new Order[Tuple2[T1,T2]] {
      def order(p1:Tuple2[T1,T2],p2:Tuple2[T1,T2]) =
        ord1(p1._1,p2._1) match {
          case Ordering.EQ => ord2(p1._2,p2._2)
          case o           => o
        }
    }
  def ternLexico[T2,T3](ord2: Order[T2], ord3: Order[T3]) =
    new Order[Tuple3[T1,T2,T3]] {
      def order(p1:Tuple3[T1,T2,T3],p2:Tuple3[T1,T2,T3]) =
        lexico(BatOrder(ord2).lexico(ord3))(
          (p1._1,(p1._2,p1._3)),
          (p2._1,(p2._2,p2._3)))
    }
}

object BatOrder {
  implicit def apply[F](order: Order[F]) = new BatOrder(order)
}

object AtomInstances {
  implicit def atomOrder[V,F](implicit
    ordInt: Order[Int],
    ordV: Order[V],
    ordF: Order[F],
    ordTs: Order[List[Atom[V,F]]]) =
    new Order[Atom[V,F]]{
      def order (x:Atom[V,F], y:Atom[V,F]): Ordering =
        (x,y) match {
          case (Var(v1),Var(v2)) => ordV(v1,v2)
          case (Var(_), Comb(_,_))  => Ordering.LT
          case (Comb(_,_), Var(_)) => Ordering.GT
          case (Comb(f1,args1),Comb(f2,args2)) =>
            BatOrder(ordInt).ternLexico(ordF,ordTs)(
              (args1.length,f1,args1),
                (args2.length,f2,args2))
        }
    }
}

case class Weight[V] private (nameMap : Map[V,Int], c: Int) {
  import Scalaz._

  def +(w: Weight[V]) =
    Weight(
      nameMap.unionWith(w.nameMap)((_: Int) + (_: Int)),
      c + w.c)

  def unary_- = Weight(nameMap ∘ (-_),-c)

  def -(w: Weight[V]) = this + -w

  def isZero = nameMap.isEmpty && c == 0

  def lowerBound =
    if (nameMap.exists { case (_,n) => n < 0 })
      None
    else Some(c)
}

object Weight {
  import Scalaz._

  def fromAtom[V,F](
    atm: Atom[V,F],
    combWeight: Comb[V,F] => Int)(implicit
    ordV: Order[V]) = {
    def wt(w: Weight[V],atms: List[Atom[V,F]]): Weight[V] =
      atms match {
        case List() => w
        case Var(name)::atms =>
          wt(
            Weight(
              w.nameMap.insertWith(name,1) {
                case (_,n) => n+1
              },w.c+1),
            atms)
        case (comb@Comb(f,args))::atms =>
          wt(Weight(
            w.nameMap,
            w.c + combWeight(comb)),args ++ atms)
      }
    implicit val ord = ordV.toScalaOrdering
    wt(Weight(new TreeMap[V,Int],-1),List(atm))
  }
}

class KnuthBendix[V,F](combWeight: Comb[V,F] => Int)(implicit
  ordInt: Order[Int], ordV: Order[V], ordF: Order[F], ordComb: Order[Comb[V,F]]) {
  case class WAtom(atm:Atom[V,F]) {
    val weight = Weight.fromAtom(atm,combWeight)
    def weightOf(atm: Atom[V,F]) = Weight.fromAtom(atm,combWeight)
    def kbOrder(otherAtm: WAtom) = {
      def weightCmp(atm1: Atom[V,F], atm2: Atom[V,F]): Option[Ordering] = {
        val w = weightOf(atm2) - weightOf(atm1)
        if (w.isZero)
          precedenceCmp(atm1,atm2)
        else if (weightDiffLess(w,atm1,atm2))
          Some(Ordering.LT)
        else if (weightDiffGreater(w,atm1,atm2))
          Some(Ordering.GT)
        else
          None
      }
      def weightLess(atm1: Atom[V,F], atm2: Atom[V,F]): Boolean = {
        val w = weightOf(atm2) - weightOf(atm1)
        if (w.isZero)
          precedenceLess(atm1,atm2)
        else weightDiffLess(w,atm1,atm2)
      }
      def weightDiffLess(w: Weight[V], atm1: Atom[V,F], atm2: Atom[V,F]): Boolean =
        w.lowerBound match {
          case None    => false
          case Some(0) => precedenceLess(atm1,atm2)
          case Some(n) => n > 0
        }
      def weightDiffGreater(w: Weight[V], atm1: Atom[V,F], atm2: Atom[V,F]) =
        weightDiffLess (-w,atm2,atm1)
      def precedenceCmp(atm1: Atom[V,F], atm2: Atom[V,F]) =
        (atm1,atm2) match {
          case (comb1@Comb(_,args1), comb2@Comb(_,args2)) =>
            ordComb(comb1,comb2) match {
              case Ordering.LT => Some(Ordering.LT)
              case Ordering.EQ =>
                (args1.view zip args2.view find {
                  case (l,r) => l != r
                }) match {
                  case Some((l,r)) => weightCmp(l,r)
                  case None        => throw new Error("Bug: precedenceCmp")
                }
              case Ordering.GT => Some(Ordering.GT)
            }
          case (_,_) => throw new Error("Bug: precedenceCmp")
        }
      def precedenceLess(atm1: Atom[V,F], atm2: Atom[V,F]): Boolean =
        (atm1,atm2) match {
          case (comb1@Comb(_,args1), comb2@Comb(_,args2)) =>
            ordComb(comb1,comb2) match {
              case Ordering.LT => true
              case Ordering.EQ =>
                (args1.view zip args2.view find {
                  case (l,r) => l != r
                }) match {
                  case Some((l,r)) => weightLess(l,r)
                  case None        => throw new Error("Bug: precdenceLess")
                }
              case Ordering.GT => false
            }
          case (_,_) => false
        }
      if (this == otherAtm)
        Some(Ordering.EQ)
      else
        weightCmp(this.atm,otherAtm.atm)
    }
  }
  object KBOrder {
    def tryCompare(atm1: WAtom, atm2: WAtom) =
      atm1.kbOrder(atm2)
  }
}
