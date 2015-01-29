package proofpeer.metis

import proofpeer.metis.util.{PartialOrder}
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/** The Resolution algorithm, supplied as streams of deduced theorems.
  *
  * @tparam V The alphabet from which variable names are drawn
  * @tparam F The alphabet from which functor names are drawn
  * @tparam P The alphabet from which predicate names are drawn
  * @param  seed Seed for randomly generating finite interpretations
  * @param  initClauses The axioms of the problem to be shown contradictory
  */
case class Resolution[V:Order,F:Order,P:Order,FV,K <: Kernel[V,F,P]](
  seed: Long,
  initClauses: List[Clause[V,F,P]],
  ithmF: IThmFactory[V,F,P,FV,K],
  interpret: Interpretation[V,F,P])
  (implicit ordFun: Order[Fun[V,F]], termOrd: PartialOrder[Term[V,F]]){

  val activeF = new ActiveFactory[V,F,P,FV,K,ithmF.type](
    ithmF,
    ithmF.litOrder,
    new Subsuming[V,F,P,Int]
  )
  val active = new activeF.Active
  val waitingF = new WaitingFactory[V,F,P,FV,K,ithmF.type](
    ithmF,
    ithmF.litOrder,
    interpret)
  val waiting = new waitingF.Waiting

  val initThms = initClauses.map(ithmF.axiom(_))
  val (initActive,factoredThms) = activeF.factor(active, initThms)
  val (s,initW) = waiting.add(0,factoredThms,100).run(
    waitingF.interpret.initState(seed))

  lazy val waitings1: Stream[(waitingF.interpret.S,waitingF.Waiting)] =
    (s,initW) #:: waitingF.interpret.preview(
      zipWith3(distances,deduced,waitings2) {
        case (Some(dist),deduced,w) => w.add(dist,deduced,0)
        case (_,_,w)                => w.point[waitingF.interpret.M]
      })(s)

  lazy val adeduced: Stream[(activeF.Active,List[ithmF.IThm])] = {
    zipWith(pulled,actives) {
      case (Some(thm),a) => activeF.add(a,thm)
      case (None,a)      => (a,List[ithmF.IThm]())
    }
  }

  lazy val actives: Stream[activeF.Active]   = initActive #:: adeduced.map(_._1)
  lazy val deduced: Stream[List[ithmF.IThm]] = adeduced.map(_._2)

  lazy val wpulled:
      Stream[(waitingF.Waiting,Option[(Double,ithmF.IThm)])] = {
    waitings1.map { case (_,w) =>
      val wpull = w.remove
      (wpull.map(_._1).getOrElse(w), wpull.map(_._2))
    }
  }
  lazy val waitings2 = wpulled.map(_._1)
  lazy val dpulled   = wpulled.map(_._2).map(unzipOption(_))
  lazy val distances: Stream[Option[Double]]     = dpulled.map(_._1)
  lazy val pulled:    Stream[Option[ithmF.IThm]] = dpulled.map(_._2)

  def zipWith[A,B,C](
    xs: Stream[A],
    ys: Stream[B])
    (f : (A,B) => C): Stream[C] = {
    xs.zip(ys).map {
      case (x,y) => f(x,y)
    }
  }

  def zipWith3[A,B,C,D](
    xs: Stream[A],
    ys: Stream[B],
    zs: Stream[C])
    (f : (A,B,C) => D): Stream[D] = {
    xs.zip(ys).zip(zs).map {
      case ((x,y),z) => f(x,y,z)
    }
  }

  def unzipOption[A,B](xy: Option[(A,B)]): (Option[A],Option[B]) = {
    xy match {
      case Some((x,y)) => (Some(x),Some(y))
      case None        => (None,   None)
    }
  }
}
