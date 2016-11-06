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
  * @param  printer for debugging
  */
case class Resolution[V:Order,F:Order,P:Order,FV,K <: Kernel[V,F,P]](
  seed: Long,
  initClauses: List[Clause[V,F,P]],
  ithmF: IThmFactory[V,F,P,FV,K],
  interpret: Interpretation[V,F,P],
  printer: Printer[V,F,P])
  (implicit ordFun: Order[Fun[V,F]], termOrd: PartialOrder[Term[V,F]]){

  val activeF = new ActiveFactory[V,F,P,FV,K,ithmF.type](
    ithmF,
    ithmF.litOrder,
    new Subsuming[V,F,P,Int],
    printer
  )
  private val active = new activeF.Active

  val waitingF = new WaitingFactory[V,F,P,FV,K,ithmF.type](
    ithmF,
    ithmF.litOrder,
    interpret)

  private val waiting = new waitingF.Waiting

  private val initThms: List[ithmF.IThm] = initClauses.map(ithmF.axiom(_))

  private val (initActive,factoredThms): (activeF.Active, List[ithmF.IThm]) =
    activeF.factor(active, initThms)

  private val (s,initW) = waiting.add(0,factoredThms,100).run(
    waitingF.interpret.initState(seed))

  /** The waiting sets immediately after a theorem is added. */
  lazy val waitingsAdded: Stream[(waitingF.interpret.S,waitingF.Waiting)] =
    (s,initW) #:: waitingF.interpret.preview(
      zipWith3(distances,deduced,waitingsRemoved) {
        case (Some(dist),deduced,w) => w.add(dist,deduced,0)
        case (_,_,w)                => w.point[waitingF.interpret.M]
      })(s)

  private lazy val actives_deduced: Stream[(activeF.Active,List[ithmF.IThm])] = {
    zipWith(nextThms,actives) {
      case (Some(thm),a) => Debug.profile("add",activeF.add(a,thm))
      case (None,a)      => (a,List[ithmF.IThm]())
    }
  }

  /** The active sets. */
  lazy val actives: Stream[activeF.Active]   =
    initActive #:: actives_deduced.map(_._1)

  /** The generations of theorems deduced by the active sets. */
  lazy val deduced: Stream[List[ithmF.IThm]] = actives_deduced.map(_._2)

  private lazy val waitings_nexts:
      Stream[(waitingF.Waiting,Option[(Double,ithmF.IThm)])] = {
    waitingsAdded.map { case (_,w) =>
      val wpull = w.remove
      (wpull.map(_._1).getOrElse(w), wpull.map(_._2))
    }
  }

  /** The waiting sets immediately after a theorem is selected for resolution. */
  lazy val waitingsRemoved = waitings_nexts.map(_._1)

  /** The theorems selected for resolution, together with their assigned distance. */
  lazy val distance_nextThms = waitings_nexts.map(_._2)

  private lazy val distance_nextThms_opt = distance_nextThms.map(unzipOption(_))

  private lazy val distances: Stream[Option[Double]]     =
    distance_nextThms_opt.map(_._1)
  private lazy val nextThms:    Stream[Option[ithmF.IThm]] =
    distance_nextThms_opt.map(_._2)

  // TODO: Move into library
  private def zipWith[A,B,C](
    xs: Stream[A],
    ys: Stream[B])
    (f : (A,B) => C): Stream[C] = {
    xs.zip(ys).map {
      case (x,y) => f(x,y)
    }
  }

  private def zipWith3[A,B,C,D](
    xs: Stream[A],
    ys: Stream[B],
    zs: Stream[C])
    (f : (A,B,C) => D): Stream[D] = {
    xs.zip(ys).zip(zs).map {
      case ((x,y),z) => f(x,y,z)
    }
  }

  private def unzipOption[A,B](xy: Option[(A,B)]): (Option[A],Option[B]) = {
    xy match {
      case Some((x,y)) => (Some(x),Some(y))
      case None        => (None,   None)
    }
  }
}
