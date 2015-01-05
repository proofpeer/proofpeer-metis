package proofpeer.metis

import scala.collection.immutable._
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
    ithms: SortedMap[Weight,(Distance,ithmFactory.IThm)]) {
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
        litWeight   = litSize + 1;
        freesWeight = cl.frees.size + 1;
        priority    = priorityFactor * ithm.id;
        trues       = score(true).toDouble;
        checks      = score(true).toDouble + score(false).toDouble;
        modelWeight = Math.pow(1 + trues/checks,modelWeightFactor)
      )
      yield distance * cl.heuristicSize * freesWeight * litWeight * modelWeight +
      priority
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
              clsFrees.traverse { case (cl,fvs) =>
                interpret.liftRand(interpret.vals.random(fvs)) >>=
                (interpret.randomPerturbation(cl,_))
              }
            perturbClauses.replicateM_(noPerturbations)
          }
          else ().point[interpret.M]

        val distance_ = distance + Math.log(ithms.length)

        val newIthms =
          ithms.foldLeftM[interpret.M,SortedMap[Weight,(Distance,ithmFactory.IThm)]](
            this.ithms) {
            case (ithms_,ithm) =>
              clauseWeight(distance_, ithm) map { w =>
                ithms_ + ( w → (distance_,ithm) )
              }
          }
        perturb >> newIthms map (new Waiting(_))
      }
    }

    def remove = {
      ithms.splitAt(1) match {
        case (head,rest) =>
          head.headOption.map(ithm => (new Waiting(rest), ithm._2))
      }
    }
  }
}
