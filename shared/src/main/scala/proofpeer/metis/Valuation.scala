package proofpeer.metis

import scala.collection.immutable._
import scalaz._
import Scalaz._

/** Random or enumerable valuations A → fin. */
case class Valuations[A](fin: FinOrd) {
  case class Valuation private[Valuations] (m: Map[A,fin.Fin]) {
    /** A success function, which allows enumeration of all valuations from zero. */
    def suc: Option[Valuation] = {
      val _ = m.keySet.foldLeft(m) {
        case (m_,x) => m(x).suc match {
          case None    => m_ + (x → fin.zero)
          case Some(n) => return Some(Valuation(m_ + (x → n)))
        }
      }
      return None
    }

    def get(x:A): Option[fin.Fin] = {
      m.get(x)
    }
  }

  /** The constant valuation to 0 */
  def zero(xs: ISet[A]) = {
    Valuation(xs.foldLeft(Map[A,fin.Fin]()) {
      case (m,x) => m + (x → fin.zero)
    })
  }

  /** The random valuation */
  def random(xs: ISet[A]) = {
    val rv = xs.foldRightM[MetisRNG.M,Map[A,fin.Fin]](Map[A,fin.Fin]()) {
      case (x,m) => for (n <- fin.random) yield m + (x → n)
    }
    rv.map(Valuation(_))
  }

  /** Fold across all valuations. */
  // TODO: Check why vf.foldValuations(Set(1,2,3,4,5,6,7),()) { case (_,_) => () }
  // blows the heap
  def foldValuations[B](xs: ISet[A], b: B)(f: (B,Valuation) => B) = {
    val vs = unfold(zero(xs)) { v => v.suc.map { v_ => (v_,v_) } }
    vs.foldLeft(b)(f)
  }
}
