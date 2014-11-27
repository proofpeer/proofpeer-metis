package proofpeer.metis

import scala.collection.immutable._
import scalaz._

case class WaitingFactory[V,F,P,S,
  K <: Kernel[V,F,P],
  ITF <: IThmFactory[V,F,P,S,K]](
  ithmFactory: ITF, litOrder: LiteralOrdering[V,F,P])(implicit
    ordInt: Order[Int],
    ordV: Order[V],
    ordF: Order[F],
    ordFun: Order[Fun[V,F]]) {

  type Weight   = Double
  type Distance = Double

  private val priorityFactor = Math.pow(10,-12)

  class Waiting private (ithms: SortedMap[Weight,(Distance,ithmFactory.IThm)]) {
    def this() {
      this(SortedMap())
    }

    // TODO: Clauses need to be additionally waited according to model-checking
    def clauseWeight(distance: Double, ithm: ithmFactory.IThm) = {
      val cl          = ithm.clause
      val litSize     = cl.lits.size
      val litWeight   = litSize + 1
      val freesWeight = cl.frees.size + 1
      val priority    = priorityFactor * ithm.id
      distance * freesWeight * litWeight +  priority
    }

    def add(distance: Double, ithms: List[ithmFactory.IThm]) = {
      val newIthms =
        ithms.foldLeft(this.ithms) {
          (ithms,ithm) =>
          val weight    = clauseWeight(distance, ithm)
          val distance_ = distance + Math.log(ithm.clause.lits.size)
          ithms + ( weight → (distance,ithm) )
        }
      new Waiting(newIthms)
    }

    def remove = {
      ithms.headOption.map(ithm => (new Waiting(ithms - ithm._1), ithm._2))
    }
  }
}
