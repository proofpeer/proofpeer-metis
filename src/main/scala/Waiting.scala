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

      distance * cl.size * freesWeight * litWeight + priority
    }

    def add(distance: Double, ithms: List[ithmFactory.IThm]) = {
      val newIthms =
        ithms.foldLeft(this.ithms) {
          (ithms,ithm) =>
          val distance_ = distance + Math.log(ithm.clause.lits.size)
          val weight    = clauseWeight(distance_, ithm)
          ithms + ( weight → (distance,ithm) )
        }
      new Waiting(newIthms)
    }

    def remove = {
      ithms.splitAt(1) match {
        case (head,rest) =>
          head.headOption.map(ithm => (new Waiting(rest), ithm._2))
      }
    }
  }
}

//ResolutionTest.resolves.take(100).foreach { case (waiting,_) => System.out.println(""); waiting.remove.get._2._2.clause.lits.foreach { case lit => System.out.println(TermPrinter.printLiteral(lit)) } }
