import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import scalaz.std._

abstract class Atom[V,F] {
  def weight(implicit ordV: Order[V]) = Weight.fromAtom(this)
}
case class Var[V,F](v: V) extends Atom[V,F]
case class Fun[V,F](f: F, args: List[Atom[V,F]]) extends Atom[V,F]

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
  implicit def apply[F](ordering: Order[F]) = new BatOrder(ordering)
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
          case (Var(_), Fun(_,_))  => Ordering.LT
          case (Fun(_,_), Var(_)) => Ordering.GT
          case (Fun(f1,args1),Fun(f2,args2)) =>
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

  def -(w: Weight[V]) = w + -w

  def isZero = nameMap.isEmpty && c == 0

  def lowerBound =
    if (nameMap.exists { case (_,n) => n < 0 })
      None
    else Some(c)
}

object Weight {
  import Scalaz._

  def fromAtom[V,F](atm: Atom[V,F])(implicit
    ordV: Order[V]) = {
    def wt(w: Weight[V],atms: List[Atom[V,F]]): Weight[V] =
      atms match {
        case List() => w
        case Var(name)::atms =>
          wt (
            Weight(
              w.nameMap.insertWith(name,0) {
                case (_,n) => n+1
              },w.c),
            atms)
      }
    implicit val ord = ordV.toScalaOrdering
    wt(Weight(new TreeMap[V,Int],-1),List(atm))
  }
}

object Atom {
  import BatOrder._
  def precedence[V,F](comb1: Fun[V,F], comb2: Fun[V,F])(implicit
    ordInt: Order[Int],
    ordF: Order[F]) =
    (comb1,comb2) match {
      case (Fun(f1,args1),Fun(f2,args2)) => ordInt.lexico(ordF)(
        (args1.length,f1),
        (args2.length,f2))
    }

  def KnuthBendix[V,F](
    atm1: Atom[V,F],
    atm2: Atom[V,F],
    precedence: (Fun[V,F],Fun[V,F]) => Ordering)
    (implicit ordV: Order[V]) = {
    def weightCmp(atm1: Atom[V,F], atm2: Atom[V,F]): Option[Ordering] = {
      val w = atm1.weight - atm2.weight
      if (w.isZero)
        precedenceCmp(atm1,atm2)
      else if (weightDiffLess(w,atm1,atm2))
        Some(Ordering.LT)
      else Some(Ordering.GT)
    }
    def weightLess(atm1: Atom[V,F], atm2: Atom[V,F]) = {
      val w = atm1.weight - atm2.weight
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
        case (fun1@Fun(f1,args1), fun2@Fun(f2,args2)) =>
         precedence(fun1,fun2) match {
           case Ordering.LT => Some(Ordering.LT)
           case Ordering.EQ =>
             (args1.view zip args2.view find {
               case (l,r) => l != r
             } map {
               case (l,r) => weightCmp(l,r)
             }).get
           case Ordering.GT => Some(Ordering.GT)
         }
      }
    def precedenceLess(atm1: Atom[V,F], atm2: Atom[V,F]): Boolean =
      (atm1,atm2) match {
        case (fun1@Fun(f1,args1), fun2@Fun(f2,args2)) =>
         precedence(fun1,fun2) match {
           case Ordering.LT => true
           case Ordering.EQ =>
             (args1.view zip args2.view find {
               case (l,r) => l != r
             } map {
               case (l,r) => weightLess(l,r)
             }).get
           case Ordering.GT => false
         }
      }
    if (atm1 == atm2)
      Some(Ordering.EQ)
    else
      weightCmp(atm1,atm2)
  }
}
