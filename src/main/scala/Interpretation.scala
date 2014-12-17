package proofpeer.metis

import scala.collection.immutable
import scalaz._
import Scalaz._

/** Finite/random interpretations of clauses. */
case class Interpretation[V,F,P] (
  maxArgs: Int,
  vals: Valuations[V]) {

  type S    = (Long,FunctorInterpretations,PredicateInterpretations)
  type M[A] = State[S,A]

  case class FiniteMappingF(m: Map[vals.fin.ListFin,vals.fin.Fin]) {
    def interpret(args: vals.fin.ListFin): M[(FiniteMappingF,vals.fin.Fin)] = {
      m.get(args) match {
        case None =>
          for (n <- liftRand(vals.fin.random))
          yield (FiniteMappingF(m + (args → n)), n)
        case Some(n) => (this, n).point[M]
      }
    }
  }

  case class FiniteMappingP(m: Map[vals.fin.ListFin,Boolean]) {
    def interpret(args: vals.fin.ListFin): M[(FiniteMappingP,Boolean)] = {
      m.get(args) match {
        case None =>
          for (b <- liftRand(MetisRNG.nextBoolean))
          yield (FiniteMappingP(m + (args → b)), b)
        case Some(b) => (this, b).point[M]
      }
    }
  }

  type FunctorInterpretations = Map[(F,Int),FiniteMappingF]
  type PredicateInterpretations = Map[(P,Int),FiniteMappingP]

  def getFIs = for (s <- get[S]) yield s._2
  def getPIs = for (s <- get[S]) yield s._3
  def addFI(farity: (F,Int), fi: FiniteMappingF) = modify[S] {
    case (seed,fis,pis) => (seed,fis + (farity → fi),pis)
  }
  def addPI(p_arity: (P,Int), pi: FiniteMappingP) = modify[S] {
    case (seed,fis,pis) => (seed,fis,pis + (p_arity → pi))
  }

  private def liftRand[A](mx:MetisRNG.M[A]): M[A] =
    for (
      s         <- get[S];
      (seed2,x) = mx.run(s._1);
      _         <- put((seed2,s._2,s._3)))
    yield x

  def interpretTerm(vl: vals.Valuation, t: Term[V,F]): M[vals.fin.Fin] = {
    t match {
      case Var(v)      => (vl.get(v).get).point[M]
      case Fun(f,args) =>
        val nargs = args.length
        if (nargs < maxArgs)
          for (
            fis           <- getFIs;
            vargs         <- args.traverse(interpretTerm(vl,_));
            fi            =  fis.getOrElse((f,nargs),(FiniteMappingF(Map())));
            newFI_value   <- fi.interpret(vals.fin.ListFin(vargs));
            _             <- addFI((f,nargs),newFI_value._1))
          yield newFI_value._2
        else {
          for (n <- liftRand(vals.fin.random))
          yield n
        }
    }
  }
}
