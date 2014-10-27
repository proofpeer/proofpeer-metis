package proofpeer.metis

import scala.collection.immutable._
import scala.language.implicitConversions
import scalaz._
import scalaz.std._
import Scalaz._

object Nets {
  abstract sealed class TermNetImpl[F,A]

  private case class Empty[F,A]() extends TermNetImpl[F,A]

  // Lookup is performed (effectively) by first flattening terms. Once flattened, the
  // net can be traversed like a string-based trie.

  // Values at a leaf of the trie
  private case class Result[F,A](results: List[A]) extends TermNetImpl[F,A]

  // A stem labelled with a term
  private case class Stem[F,A](term:Term[Unit,(F,Int)],net:TermNetImpl[F,A])
      extends TermNetImpl[F,A]

  // Branch: there might be a branch for a variable, with the other branches labelled
  // for functor names and arity.
  private case class Branch[F,A](
    net:Option[TermNetImpl[F,A]],
    arityMap:Map[(F,Int),TermNetImpl[F,A]]) extends TermNetImpl[F,A]

  /** A trie-like map from quotiented terms (all variables are made distinct) to
    * sets of values.
    *
    * @tparam F the alphabet from which functor names are drawn
    * @tparam A the type of values
    */
  class TermNet[F,A] private (termNet: TermNetImpl[F,A], theSize: Int) {

    /** Create an empty map. */
    def this() {
      this(Empty[F,A],0)
    }

    /** Return true if the map is empty. */
    def isEmpty = termNet match {
      case Empty() => true
      case _       => false
    }

    /** The number of values held in the map. Note that a single term may have one
        or more values. */
    val size = this.theSize

    private def insertNet(
      net1: TermNetImpl[F,A],
      args: List[Term[Unit,(F,Int)]],
      net2: TermNetImpl[F,A]): TermNetImpl[F,A] = {
      (net1, args, net2) match {
        case (_, args, Empty()) =>
          args.foldRight(net1) {
            case (tm,stemNet) => Stem(tm,stemNet)
        }
        case (Result(xs), List(), Result(ys)) => Result(xs ++ ys)
        case (net1, arg :: args, Stem(tm,net2)) if arg == tm =>
          Stem(tm,insertNet(net1,args,net2))
        case (net1, args, Stem(tm,net2)) =>
          insertNet(net1,args,insertNet(net2,List(tm),Branch(None,Map())))
        case (net1, Var(()) :: args, Branch(vnet, fmap)) =>
          Branch(Some(insertNet(net1,args,vnet.getOrElse(Empty()))), fmap)
        case (net1, Fun(f,fargs) :: args, Branch(vnet, fmap)) =>
          val fnet =
            insertNet(net1, fargs ++ args, fmap.getOrElse(f,Empty()))
          Branch(vnet, fmap + (f → fnet))
        case _ => throw new Error("Bug: insertNet")
      }
    }

    private def computeSize(net: TermNetImpl[F,A]): Int = {
      net match {
        case Empty()      => 0
        case Result(xs)   => xs.length
        case Stem(_,net2) => computeSize(net2)
        case Branch(vnet,fnet) =>
          vnet.map(computeSize(_)).getOrElse(0) +
          fnet.foldLeft(0){
            case (sz,(_,net2)) => sz + computeSize(net2)
          }
      }
    }

    private def filterNet(pred: A => Boolean, net: TermNetImpl[F,A]):
        TermNetImpl[F,A] = {
      net match {
        case Empty() => Empty()
        case Result(xs) => xs.filter(pred) match {
          case Nil => Empty()
          case xs  => Result(xs)
        }
        case Stem(tm,net2) => filterNet(pred, net2) match {
          case Empty() => Empty()
          case net2    => Stem(tm,net2)
        }
        case Branch(vnet,fnet) =>
          val vnet2 = vnet.map(filterNet(pred,_))
          val fnet2 =
            fnet.mapValues(filterNet(pred, _))
              .filter {
              case (_,Empty()) => false
              case _           => true
            }
          if (fnet2.isEmpty) {
            vnet2 match {
              case None          => Empty()
              case Some(Empty()) => Empty()
              case Some(vnet2)   => Stem(Var[Unit,(F,Int)](()),vnet2)
            }
          }
          else
            Branch(vnet2,fnet2)
      }
    }

    /** Filter values from the map. */
    def filter(pred: A => Boolean) = {
      val termNet = filterNet(pred,this.termNet)
      new TermNet(termNet,computeSize(termNet))
    }

    private def qTerm[V](tm: Term[V,F]): Term[Unit,(F,Int)] = {
      tm match {
        case Var(_) => Var(())
        case Fun(f,args) => Fun((f,args.length),args.map(qTerm(_)))
      }
    }

    /** Insert/append a new value into the map for a given pattern. All variables in
      * the pattern will be (effectively) renamed so that all are unique. */
    def insert[V](pat: Term[V,F],x: A): TermNet[F,A] = {
      new TermNet(
        insertNet(Result(List(x)),List(qTerm(pat)),this.termNet),
        this.size + 1)
    }

    // Hurd makes this tail-recursive.
    private def termMatchesQPat[V](term: Term[V,F], qpat: Term[Unit,(F,Int)]):
        Boolean =
      (term,qpat) match {
        case (_,Var(())) => true
        case (Fun(f1,args),Fun((f2,_),qpats)) if f1 == f2 =>
          args.view.zip(qpats).map {
            case (arg,qpat2) => termMatchesQPat(arg,qpat2)
          }.foldLeft(true) { _ && _ }
        case _ =>
          false
      }

    private def matches[V](args: List[Term[V,F]],net:TermNetImpl[F,A]): List[A] =
      (args,net) match {
        case (List(),Result(xs)) => xs
        case (arg::args,Stem(pat,net2)) =>
          if (termMatchesQPat(arg,pat))
            matches(args,net2)
          else List()
        case (arg::args,Branch(vnet,fmap)) =>
          val vresults = vnet.map(matches(args,_)).getOrElse(List())
          val fresults = arg match {
              case Fun(f,fargs) =>
                fmap.get((f,fargs.length))
                  .map(matches(fargs++args,_)).getOrElse(List())
              case _ => List()
            }
          vresults++fresults
        case _ => throw new Error("Bug: matches")
      }

    /** Return all values where tm matches a key-pattern. */
    def matches[V](tm: Term[V,F]): List[A] = {
      this.termNet match {
        case Empty() => List()
        case net     => matches(List(tm),net)
      }
    }

    type QSubst[V] = PartialFunction[V,Term[Unit,(F,Int)]]
    private def qTermMatchesPat[V](
      θ: QSubst[V],
      qtm:Term[Unit,(F,Int)],
      pat:Term[V,F]):
        Option[QSubst[V]] = {
      (qtm,pat) match {
        case (_,(Var(v))) =>
          θ.lift(v) match {
          case None       => Some(θ.orElse(Map() + { v → qtm }))
          case Some(qtm2) if qtm == qtm2 => Some(θ)
          case _          => None
        }
        case (Fun(f,fargs),Fun(g,pats)) if f == (g,pats.length) =>
          fargs.zip(pats).foldLeftM(θ) {
            case (θ,(arg,pat)) => qTermMatchesPat(θ,arg,pat)
          }
        case (_,_) => None
      }
    }

    // Grab a list of the next possible n-length chains of arguments and the nets
    // which follow them.
    private def nextArgs[V](n:Int,net:TermNetImpl[F,A]):
        List[(List[Term[Unit,(F,Int)]],TermNetImpl[F,A])] = {
      if (n == 0) {
        return List((List(),net))
      }
      else {
        net match {
          case Stem(tm,net2) =>
            for ((args,net3) <- nextArgs(n - 1,net2))
            yield (tm::args,net3)
          case Branch(vnet,fnet) =>
            val vargsNet = vnet match {
              case None       => List()
              case Some(net2) =>
                for ((args,net3) <- nextArgs(n - 1,net2))
                yield (Var[Unit, (F,Int)](())::args, net3)
            }
            val fargsNet =
              for
                (((f,arity),net2) <- fnet.iterator;
                (fargs,net3) <- nextArgs(arity,net2);
                  (args,net4) <- nextArgs(n - 1,net3))
                  yield (Fun((f,arity),fargs)::args, net4)
            vargsNet ++ fargsNet
          case _ => List()
        }
      }
    }

    // Grab a list of the next possible arguments in the net and the nets which
    // follow.
    private def nextArg[V](net:TermNetImpl[F,A]):
        List[(Term[Unit,(F,Int)],TermNetImpl[F,A])] = {
      nextArgs(1,net).map {
        case (List(nextTm),net2) => (nextTm,net2)
        case _                   => throw new Error("Bug: nextArg")
      }
    }

    // This code is simpler than Hurd's, but is probably very suboptimal. Note
    // that we retrieve all possible nextTerms and *then* check whether each is
    // equal to the variable bound to v in θ. In case of unequal terms, Hurd's
    // solution is more opportunistic, not returning nextTerms as soon as they
    // are known to be unequal to the sought term. (see foldEqualTerms).
    private def matched[V](
      θ: QSubst[V],
      pats: List[Term[V,F]],
      net:TermNetImpl[F,A]): List[(QSubst[V],List[A])] = {
      (pats,net) match {
        case (List(),Result(xs)) => List((θ,xs))
        case (Var(v)::pats,net) => {
          val boundTo = θ.lift(v)
          (for ((next,net2) <- nextArg(net))
          yield boundTo match {
            case None => matched(θ.orElse (Map() + (v → next)), pats, net2)
            case Some(tm) if next == tm => matched(θ, pats, net2)
            case _ => List()
          }).flatten
        }
        case (pat::pats,Stem(arg,net2)) =>
          qTermMatchesPat(θ,arg,pat).map {
            θ2 => matched(θ2,pats,net2)
          }.getOrElse(List())
        case (pat::pats,Branch(vnet,fnet)) =>
          val vmatches = vnet match {
            case None => List()
            case Some(vnet) =>
            qTermMatchesPat(θ,Var(()),pat).map {
              case vθ => matched(vθ,pats,vnet)
            }.getOrElse(List())
          }
          val fmatches = pat match {
            case Fun(f,fargs) =>
              fnet.lift((f,fargs.length)).map {
                fnet => matched(θ,fargs++pats,fnet)
              }.getOrElse(List())
            case _ => List()
          }
          vmatches ++ fmatches
        case _ => throw new Error("Bug: matched")
      }
    }

    /** Return all values where the key matches the given pattern. */
    def matched[V](pat: Term[V,F]): List[A] = {
      this.termNet match {
        case Empty() => List()
        case net     =>
          matched(Map(), List(pat), net).map(_._2).flatten
      }
    }

    // Tail-recursive version
    // private def netMatches[V,F,A](
    //   acc: List[A],
    //   agenda: List[(List[Term[V,F]],TermNetImpl[F,A])]): List[A] = {
    //   agenda match {
    //     case List() => acc
    //     case ((List(),Result(xs)) ::rest) => netMatches(xs ++ acc,rest)
    //     case ((arg::args, Stem(arg2,net2))::rest) if matchesQTerm(arg,arg2) =>
    //       netMatches(acc,(args,net2)::rest)
    //     case ((arg::args, Branch(vnet,fmap))::rest) =>
    //       val vagenda = vnet.map((args,_))
    //       val fagenda =
    //         arg match {
    //           case Fun(f,fargs) =>
    //             fmap.get((f,fargs.length)).map(((fargs ++ args),_))
    //           case _ => None
    //         }
    //       netMatches(acc,List()++vagenda++fagenda++rest)
    //   }
    // }

    private def canUnifyQTerms(qtm:  Term[Unit,(F,Int)],qtm2: Term[Unit,(F,Int)]):
    Boolean = {
      (qtm,qtm2) match {
        case (Var(()),_) => true
        case (_,Var(())) => true
        case (Fun((f,m),fargs), Fun((g,n),gargs)) =>
         f == g && m == n && fargs.view.zip(gargs).foldLeft(true) {
           case (b,(arg1,arg2)) => b && canUnifyQTerms(arg1,arg2) }
      }
    }

    private def unifiesQTerm[V](
      θ: QSubst[V],
      tm: Term[V,F],
      qtm: Term[Unit,(F,Int)]): Option[QSubst[V]] = {
      (tm,qtm) match {
        case (Var(v),qtm) => θ.lift(v) match {
          case None                                =>
            Some(θ.orElse(Map() + (v → qtm)))
          case Some(qtm2) if canUnifyQTerms(qtm,qtm2) => Some(θ)
          case _                                      => None
        }
        case (tm,Var(())) => Some(θ)
        case (Fun(f1,fargs),Fun((f2,_),qargs)) if f1 == f2 =>
          fargs.zip(qargs).foldLeftM(θ) {
            case (θ,(farg,qarg)) =>
              unifiesQTerm(θ,farg,qarg)
          }
      }
    }

    // As with the code for matched above, this is simpler and less efficient than
    // Hurd's. Again, we retrieve the nextTerm and *then* check whether it unifies
    // with the variable bound to v in θ. In case of non-unifying terms, Hurd's
    // solution is more opportunistic, failing early as the nextArg is computed
    // (see foldUnifiableTerms). Note this for potential future optimisations.
    private def unifies[V](
      θ: QSubst[V],
      args: List[Term[V,F]],
      net: TermNetImpl[F,A]): List[A] = {
      (args,net) match {
        case (List(),Result(xs)) => xs
        case (Var(v)::args,net) =>
          (for ((nextQtm,net2) <- nextArg(net))
          yield θ.lift(v) match {
            case None =>
              unifies(
                θ.orElse(Map() + (v → nextQtm)),
                args,
                net2)
            case Some(qtm) if qtm == nextQtm =>
              unifies(θ,args,net2)
            case _ => List()
          }).flatten
        case (arg::args,Stem(qtm,net2)) =>
          unifiesQTerm(θ,arg,qtm)
            .map(unifies(_,args,net2)).getOrElse(List())
        case ((arg@Fun(f,fargs))::args,Branch(vnet,fnet)) =>
          val vresults = vnet match {
            case Some(vnet) =>
              (for ((nextQtm,net2) <- nextArg(vnet))
              yield
                unifiesQTerm(θ,arg,nextQtm)
                .map(unifies(_,args,net2)).getOrElse(List())).flatten
            case None => List()
          }
          val fresults = fnet.lift((f,fargs.length)) match {
            case Some(fnet2) => unifies(θ,fargs,fnet2)
            case None        => List()
          }
          vresults ++ fresults
        case _ => List()
      }
    }

    /** Return all values where tm can be unified with the key. */
    def unifies[V](tm: Term[V,F]):
        List[A] = {
      unifies(Map(),List(tm),this.termNet)
    }
  }

  /** A factor for trie-like maps from atoms to values.
    *
    * @tparam F the alphabet from which functor names are drawn
    * @tparam P the alphabet from which predicate names are drawn
    * @tparam A the type of values
    *
    * @param predicatesAreFunctors Internally, an AtomNet is just a TermNet. To
    * accommodate this, we need a one-one function from predicates to functors.
    * This is then legal, since predicates can only occur at the head position of a
    * term.
    * @param eqFunctor As above, we need a functor to represent equality, which is
    * distinct from all other predicate functors.
  */
  class AtomNet[F,P,A](predicatesAreFunctors: P=>F, eqFunctor: F) {
    private[Nets] def atomToTerm[V](atm: Atom[V,F,P]) = {
      atm match {
        case Pred(p,args) => Fun(predicatesAreFunctors(p),args)
        case Eql(lhs,rhs) => Fun(eqFunctor,List(lhs,rhs))
      }
    }

    class AtomNet private (termNet: TermNet[F,A]) {
      def this() {
        this(new TermNet)
      }

      def insert[V](atm:Atom[V,F,P], x:A) =
        new AtomNet(termNet.insert(atomToTerm(atm),x))
      def filter(pred: A=>Boolean) = new AtomNet(termNet.filter(pred))
      def unifies[V](atm: Atom[V,F,P]) = termNet.unifies(atomToTerm(atm))
      def matches[V](atm: Atom[V,F,P]) = termNet.matches(atomToTerm(atm))
      def matched[V](atm: Atom[V,F,P]) = termNet.matched(atomToTerm(atm))
      def isEmpty = termNet.isEmpty
      def size    = termNet.size
    }
  }

  /** A factor for trie-like maps from literals to values.
    *
    * @tparam F the alphabet from which functor names are drawn
    * @tparam P the alphabet from which predicate names are drawn
    * @tparam A the type of values
    *
    * @param predicatesAreFunctors Internally, a LiteralNet is just a TermNet. To
    * accommodate this, we need a one-one function from predicates to functors.
    * @param eqFunctor As above, we need a functor to represent equality, which is
    * distinct from all other predicate functors.
    * @param negFunctor As above, we need a functor to represent negation, which is
    * distinct from all other predicate functors.
  */
  def LiteralNet[F,P,A](predicatesAreFunctors: P=>F, eqFunctor: F, negFunctor: F) {
    val atomNetF = new AtomNet(predicatesAreFunctors,eqFunctor)
    class LiteralNet private (termNet: TermNet[F,A]) {
      private def litToTerm[V](lit: Literal[V,F,P]) = {
        lit match {
          case Literal(true,atm)  => atomNetF.atomToTerm(atm)
          case Literal(false,atm) => Fun(negFunctor,List(atomNetF.atomToTerm(atm)))
        }
      }

      /** Create an empty map. */
      def this() {
        this(new TermNet)
      }

      def insert[V](lit:Literal[V,F,P], x:A) =
        new LiteralNet(termNet.insert(litToTerm(lit),x))
      def filter(pred: A=>Boolean) = new LiteralNet(termNet.filter(pred))
      def unifies[V](lit: Literal[V,F,P]) = termNet.unifies(litToTerm(lit))
      def matches[V](lit: Literal[V,F,P]) = termNet.matches(litToTerm(lit))
      def matched[V](lit: Literal[V,F,P]) = termNet.matched(litToTerm(lit))
      def isEmpty = termNet.isEmpty
      def size    = termNet.size
    }
  }
}
