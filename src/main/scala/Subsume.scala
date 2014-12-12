package proofpeer.metis

import Nets._
import scala.collection.immutable.HashMap
import scala.collection.SeqView
import scalaz._
import Scalaz._

class Subsumer[V,F,P,A](implicit ordV: Order[V]) {
  type Id   = Int
  type Size = Int
  private case class NonUnit (
    nextId:  Int,
    clauses: Map[Int,(List[Literal[V,F,P]], Clause[V,F,P],A)],
    fstLits: LiteralNet[F,P,(Id,Size)],
    sndLits: LiteralNet[F,P,(Id,Size)]) {
    def this() {
      this(0, Map.empty, new LiteralNet(), new LiteralNet())
    }
  }

  // lits1 is a sequence of literals from the potentially subsuming clause.
  def subsumes(
    lits1: List[Literal[V,F,P]],
    lits2: List[Literal[V,F,P]]): Boolean = {

    def search(σ: Subst[V,Term[V,F]], σss: List[List[Subst[V, Term[V, F]]]]):
        Boolean = {
      σss match {
        case List()        => true
        case List(_)::σss_ => search(σ, σss_)
        case σs::σss_      =>
          for (
            σ_    <- σs;
            nextσ <- σ.union(σ_).toList)
            return search(nextσ, σss_)
          return false
      }
    }

    // θ is used to shortcut the search when an impossible bottleneck is found.
    var θ = Subst.empty[V,Term[V,F]]
    val σss = {
      for (lit2 <- lits2)
      yield {
        for (
          lit1 <- lits1;
          σ    <- lit1.patMatch(θ, lit2))
        yield σ }.toList match {
        case List()  => return false
        case List(σ) => θ = σ; List(σ)
        case σs      => σs
      }
    }.sortBy(_.length)

    search(θ, σss.toList)
  }

  class Subsume private[Subsumer] (
    containsEmpty: Boolean,
    units:         LiteralNet[F,P,(Literal[V,F,P],Clause[V,F,P],A)],
    nonunits:      NonUnit) {
    def this() {
      this(false, new LiteralNet(), new NonUnit())
    }

    private def sortLits(lits: List[Literal[V,F,P]]) = {
      lits.toSeq.sortBy(_.heuristicSize)(scala.math.Ordering.Int.reverse)
    }

    private def withSyms(lits: Set[Literal[V,F,P]]) = {
      lits ++ (
        for (Literal(isPositive,Eql(x,y)) <- lits)
        yield Literal(isPositive,Eql[V,F,P](y,x))
      )
    }

    private def incompatible(lit1: Literal[V,F,P]) = {
      val lits = withSyms(Set(lit1))

      lit2: Literal[V,F,P] => lits.exists(
        _.unify(Subst.empty[V,Term[V,F]],lit2).isEmpty)
    }

    private def picks[A](xs: List[A]): SeqView[(A, List[A]),Seq[_]] = {
      def picksAcc(
        init: List[A],
        tail: List[A],
        acc: SeqView[(A, List[A]),Seq[_]]):
          SeqView[(A, List[A]),Seq[_]] = {
        tail match {
          case List() => acc
          case y::ys  => picksAcc(y::init,ys,(y,init++ys) +: acc)
        }
      }
      picksAcc(List(),xs,List().view)
    }

    def insert(cl: Clause[V,F,P], x:A) = {
      // Sort the literals from largest to smallest
      // (Larger literals will fail subsumption checking earlier.)
      sortLits(cl.lits.toList) match {
        case List() => new Subsume(
          true,
          this.units,
          this.nonunits)
        case List(lit) => new Subsume(
          this.containsEmpty,
          this.units.insert(lit,(lit,cl,x)),
          this.nonunits)
        case fstLit::sndLit::rstLits =>
          val id   = this.nonunits.nextId
          val size = cl.lits.size
          val fstLits = this.nonunits.fstLits.insert(fstLit,(id,size))
          val pred = incompatible(fstLit)

          // Try to find a better candidate for the second literal, one which does
          // not match the first.
          val (sndLit_,rstLits_) =
            (picks(sndLit::rstLits) filter { case (lit,_) => pred(lit) }).
              find(_ => true) getOrElse (sndLit,rstLits)

          val sndLits = this.nonunits.sndLits.insert(sndLit_,(id,size))
          new Subsume(
            this.containsEmpty,
            this.units,
            new NonUnit(
              id+1,
              // insert the fstLit and sndLit at the back since at least one of them
              // will be guaranteed to give a valid substitution in the main
              // subsumption checking code. We want to fail as early as possible.
              this.nonunits.clauses +
                (id → ((rstLits_ :+ fstLit :+ sndLit_,cl,x))),
              fstLits,
              sndLits))
      }
    }

    def filter(p: A=>Boolean) = {
      val unit = this.units.filter { case (_,_,x) => p(x) }
      val nonunit = {
        val cls = this.nonunits.clauses.filter { case (_,(_,_,x)) => p(x) }
        if (cls.size == this.nonunits.clauses.size)
          this.nonunits
        else {
          val fstLits = this.nonunits.fstLits.filter {
            case (id,_) => cls.contains(id)
          }
          val sndLits = this.nonunits.fstLits.filter {
            case (id,_) => cls.contains(id)
          }
          new NonUnit(
            this.nonunits.nextId,
            cls,
            fstLits,
            sndLits)
        }
      }
      new Subsume(containsEmpty,unit,nonunit)
    }

    def isStrictlySubsumed(cl: Clause[V,F,P]): Boolean = {
      val size = cl.lits.size

      if (this.containsEmpty)
        return true

      val lits    = cl.lits
      val litSyms = withSyms(lits)

      if (size == 0)
        return false

      if (litSyms.view.exists { this.units.matches(_).nonEmpty })
        return true

      if (size == 1)
        return false

      for (
        lit         <- litSyms;
        (id,size_)  <- this.nonunits.fstLits.matches(lit)
        ++ this.nonunits.sndLits.matches(lit);
        if size_ <= size;
        (lits_,_,_) = this.nonunits.clauses(id);
        if subsumes(lits_,litSyms.toList))
        return true

      return false
    }
  }
}
