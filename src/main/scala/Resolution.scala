package proofpeer.metis

import scala.language.implicitConversions
import scalaz._
import Scalaz._

object Resolution {
  implicit val ordFun = new Order[Fun[String,String]] {
    def order(f: Fun[String,String], g: Fun[String,String]) = {
      (f.args.length,f.f) ?|? (g.args.length,g.f)
    }
  }

  val kbo    = new KnuthBendix[String,String]((_,_) => 1)
  val litOrd = new LiteralOrdering[String,String,String](kbo,x => x)
  val kernel = new Kernel[String,String,String]
  val factor = new Factor[String,String,String]
  val ithmF  = new IThmFactory[String,String,String,Int,kernel.type](
    kernel,
    0,
    n => (n+1, "genvar" + n),
    litOrd,
    factor)
  val vals = new Valuations[String](FinOrd(8))
  val interpret = new Interpretation[String,String,String](1000,vals)
  val activeF = new ActiveFactory[String,String,String,Int,kernel.type,ithmF.type](
    ithmF,
    litOrd)
  val active = new activeF.Active
  val waitingF = new WaitingFactory[String,String,String,Int,kernel.type,ithmF.type](
    ithmF,
    litOrd,
    interpret)
  val waiting = new waitingF.Waiting

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

  class system(s: waitingF.interpret.S, initThms: List[ithmF.IThm]) {
    lazy val waitings1: Stream[(waitingF.interpret.S,waitingF.Waiting)] =
      (s,waiting) #:: waitingF.interpret.preview(
        zipWith3(distances,deduced,waitings2) {
            case (Some(dist),deduced,w) => w.add(dist,deduced,0)
            case (_,_,w)                => w.point[waitingF.interpret.M]
        })(s)

    lazy val adeduced: Stream[(activeF.Active,List[ithmF.IThm])] = {
      zipWith(pulled,actives) {
        case (Some(thm),a) => a.add(thm)
        case (None,a)      => (a,List[ithmF.IThm]())
      }
    }

    lazy val actives: Stream[activeF.Active]   = active   #:: adeduced.map(_._1)
    lazy val deduced: Stream[List[ithmF.IThm]] = initThms #:: adeduced.map(_._2)

    lazy val wpulled:
        Stream[(waitingF.Waiting,Option[(Double,ithmF.IThm)])] = {
      waitings1.map { case (_,w) =>
        val wpull = w.remove
        (wpull.map(_._1).getOrElse(w), wpull.map(_._2))
      }
    }
    lazy val waitings2 = wpulled.map(_._1)
    lazy val dpulled   = wpulled.map(_._2).map(unzipOption(_))
    lazy val distances: Stream[Option[Double]]     =
      Some[Double](0) #:: dpulled.map(_._1)
    lazy val pulled:    Stream[Option[ithmF.IThm]] = dpulled.map(_._2)
  }

  def main(args: Array[String]) {
    val problem = testing.tptp.Axioms.SET001_MINUS1.clauses.map { lits =>
      Resolution.ithmF.axiom(Clause(lits.toSet))
    }
    val sys = new Resolution.system(
      Resolution.waitingF.interpret.run(1.point[Resolution.waitingF.interpret.M])._1,
      problem)
    sys.waitings2(0)
  }
}
