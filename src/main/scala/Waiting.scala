package proofpeer.metis

import scala.collection.immutable._
import scala.collection.immutable.MapLike
import scala.collection.SetLike
import scala.collection.TraversableLike
import scalaz._
import Scalaz._

case class WaitingFactory[V:Order,F:Order,P,S,
  K <: Kernel[V,F,P],
  ITF <: IThmFactory[V,F,P,S,K]](
  ithmFactory: ITF,
    litOrder: LiteralOrdering[V,F,P],
    interpret: Interpretation[V,F,P])(
  implicit ordFun: Order[Fun[V,F]]) {

  type Weight   = Double
  type Distance = Double

  private val priorityFactor    = Math.pow(10,-12)
  private val modelWeightFactor = 1.0
  private val maxChecks         = Some(20)

  class Waiting private (
    ithms: SortedMap[Weight,List[(Distance,ithmFactory.IThm)]]) {
    def this() {
      this(SortedMap())
    }
    // DEBUG
    def theIthms = ithms

    def clauseWeight(distance: Double, ithm: ithmFactory.IThm) = {
      for (
        score       <- interpret.checkClause(maxChecks,ithm.clause);
        cl          = ithm.clause;
        litSize     = cl.lits.size;
        litWeight   = litSize;
        freesWeight = cl.frees.size + 1;
        priority    = priorityFactor * ithm.id;
        trues       = score(true).toDouble;
        checks      = score(true).toDouble + score(false).toDouble;
        modelWeight = Math.pow(1 + trues/checks,modelWeightFactor);
        weight      = distance * cl.heuristicSize * freesWeight * litWeight *
                      1.0 + priority
//                      modelWeight + priority
      )
      yield {
        // System.out.println("==========")
        // Debug.printClause(ithm.clause.lits)
        // System.out.println("dist: " + distance)
        // System.out.println("symbolsW: " + cl.heuristicSize)
        // System.out.println("variablesW: " + freesWeight)
        // System.out.println("literalsW: " + litWeight)
        // System.out.println("modelsW: " + modelWeight)
        // System.out.println("weight: " + weight)
        // System.out.println("==========")
        weight
      }
    }

    def add(
      distance: Double,
      ithms: List[ithmFactory.IThm],
      noPerturbations: Int) = {

      if (ithms.isEmpty)
        this.point[interpret.M]
      else {
        val perturb =
          if (noPerturbations > 0) {
            val clsFrees = ithms.map { cl => (cl.clause, cl.clause.frees) }
            val perturbClauses =
              clsFrees.traverse[interpret.M,Unit] { case (cl,fvs) =>
                for (
                  rv <- interpret.liftRand(interpret.vals.random(fvs));
                  v  <- interpret.interpretClause(rv,cl);
                  _  <- if (v) interpret.randomPerturbation(cl,rv)
                        else ().point[interpret.M])
                yield ()
              }
            perturbClauses.replicateM_(noPerturbations)
          }
          else ().point[interpret.M]

        val distance_ = distance + Math.log(ithms.length)

        val newIthms =
          ithms.foldLeftM[
            interpret.M,
            SortedMap[Weight,List[(Distance,ithmFactory.IThm)]]](this.ithms) {
            case (ithms_,ithm) =>
              clauseWeight(distance_, ithm) map { w =>
                val wthms = ithms_.getOrElse(w,List())
                ithms_ + ( w → ((distance_,ithm)::wthms))
              }
          }
        perturb >> newIthms map (new Waiting(_))
      }
    }

    object EmptyMap {
      def unapply(map: SortedMap[Weight,List[(Distance,ithmFactory.IThm)]]) =
        map.isEmpty
    }

    object MapCons {
      def unapply(map: SortedMap[Weight,List[(Distance,ithmFactory.IThm)]]) = {
        val (head,rest) = map.splitAt(1)
        head.headOption.map { case h => (h,rest) }
      }
    }

    def remove = {
      ithms match {
        case EmptyMap() => None
        case MapCons((w,List(dithm)), rest) =>
          Some((new Waiting(rest),dithm))
        case MapCons((w,dithm::dithms), rest) =>
          Some((new Waiting(rest + (w → dithms)), dithm))
        case _ => throw new Error("Bug: Waiting.remove")
      }
    }
  }
}
