package proofpeer.metis

import scala.collection.immutable.Map
import scala.collection.SeqView
import scalaz._
import Scalaz._

import LiteralInstances._
import Nets._

// TODO: A is currently unused. Provided in Metis for extra filtering.
class Subsuming[V:Order, F: Order, P: Order, A] {
  type Id   = Int
  type Size = Int
  case class NonUnit private[Subsuming](
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
          return false // TODO: Bad style
      }
    }

    // θ is used to shortcut the search when an impossible bottleneck is found.
    var (θ:Option[Subst[V,Term[V,F]]]) = Some(Subst.empty[V,Term[V,F]])
    val σss = {
      for (
        lit1 <- lits1;
        θ_   <- θ.toList)
      yield {
        for (
          lit2 <- lits2;
          σ    <- lit1.patMatch(θ_, lit2))
        yield σ }.toList match {
        case List()  => return false // TODO: Bad style
        case List(σ) => θ = θ_.union(σ); List(σ)
        case σs      => σs
      }
    }.sortBy(_.length)

    (θ.map(search(_, σss.toList))).getOrElse(false)
  }

  // We don't really need to store the clause in the literal net. Used
  // with A for additional filtering in METIS.
  case class Subsume private[Subsuming] (
    containsEmpty: Boolean,
    units:         LiteralNet[F,P,(Literal[V,F,P],Clause[V,F,P],A)],
    nonunits:      NonUnit) {
    def this() {
      this(false, new LiteralNet(), new NonUnit())
    }

    private def sortLits(lits: List[Literal[V,F,P]]) = {
      lits.toSeq.sortBy(_.heuristicSize)(scala.math.Ordering.Int.reverse)
    }

    private def withSyms(lits: ISet[Literal[V,F,P]]) =
      lits.foldMap[ISet[Literal[V,F,P]]] {
        case lit@Literal(isPositive,Eql(x,y)) =>
          ISet.fromList(List(lit,Literal(isPositive,Eql[V,F,P](y,x))))
        case lit => ISet.singleton(lit)
      }

    private def incompatible(lit1: Literal[V,F,P]) = {
      val lits = withSyms(ISet.singleton(lit1))

      lit2: Literal[V,F,P] => lits.any(
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
            NonUnit(
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
          NonUnit(
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
      val litSyms = withSyms(lits).toList

      if (size == 0)
        return false

      if (litSyms.exists { lit =>
        this.units.matches(lit).find {
          case (lit_,_,_) =>
            !lit_.patMatch(Subst.empty[V,Term[V,F]],lit).isEmpty
        }.isDefined
      })
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
