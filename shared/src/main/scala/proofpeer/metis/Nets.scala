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

  private def insertNet[F,A](
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

  type QSubst[V,F] = Map[V,Term[Unit,(F,Int)]]

  private def canUnifyQTerms[F](qtm:  Term[Unit,(F,Int)],qtm2: Term[Unit,(F,Int)]):
      Boolean = {
    (qtm,qtm2) match {
      case (Var(()),_) => true
      case (_,Var(())) => true
      case (Fun((f,m),fargs), Fun((g,n),gargs)) =>
        f == g && m == n && fargs.view.zip(gargs).foldLeft(true) {
          case (b,(arg1,arg2)) => b && canUnifyQTerms(arg1,arg2) }
    }
  }

  private def unifiesQTerm[V,F](
    θ: QSubst[V,F],
    tm: Term[V,F],
    qtm: Term[Unit,(F,Int)]): Option[QSubst[V,F]] = {
    (tm,qtm) match {
      case (Var(v),qtm) => θ.lift(v) match {
        case None => Some(θ + (v → qtm))
        case Some(qtm2) if canUnifyQTerms(qtm,qtm2) => Some(θ)
        case _ => None
      }
      case (tm,Var(())) => Some(θ)
      case (Fun(f1,fargs),Fun((f2,_),qargs))
          if f1 == f2 && fargs.length == qargs.length =>
        fargs.zip(qargs).foldLeftM(θ) {
          case (θ,(farg,qarg)) =>
            unifiesQTerm(θ,farg,qarg)
        }
      case _ => None
    }
  }

  // Grab a list of the next possible n-length chains of arguments and the nets
  // which follow them.
  private def nextArgs[V,F,A](n:Int,net:TermNetImpl[F,A]):
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
                (fargs,net3)    <- nextArgs(arity,net2);
                (args,net4)     <- nextArgs(n - 1,net3))
            yield (Fun((f,arity),fargs)::args, net4)
          vargsNet ++ fargsNet
        case _ => List()
      }
    }
  }

  // Grab a list of the next possible arguments in the net and the nets which
  // follow.
  private def nextArg[V,F,A](net:TermNetImpl[F,A]):
      List[(Term[Unit,(F,Int)],TermNetImpl[F,A])] = {
    nextArgs(1,net).map {
      case (List(nextTm),net2) => (nextTm,net2)
      case _                   => throw new Error("Bug: nextArg")
    }
  }

  // As with the code for matched above, this is simpler and less efficient than
  // Hurd's. Again, we retrieve the nextTerm and *then* check whether it unifies
  // with the variable bound to v in θ. In case of non-unifying terms, Hurd's
  // solution is more opportunistic, failing early as the nextArg is computed
  // (see foldUnifiableTerms). Note this for potential future optimisations.
  private def unifiesArgs[V,F,A](
    θ: QSubst[V,F],
    args: List[Term[V,F]],
    net: TermNetImpl[F,A]): List[A] = {
    (args,net) match {
      case (List(),Result(xs)) => xs
      case (arg::args, _) =>
        (for ((nextQtm,net2) <- nextArg(net))
        yield unifiesQTerm(θ,arg,nextQtm)
          .map(unifiesArgs(_,args,net2)).getOrElse(List())).flatten
      case _ => List()
    }
  }

  private def qTerm[V,F](tm: Term[V,F]): Term[Unit,(F,Int)] = {
    tm match {
      case Var(_) => Var(())
      case Fun(f,args) => Fun((f,args.length),args.map(qTerm(_)))
    }
  }

  // Hurd makes this tail-recursive.
  private def termMatchesQPat[V,F](term: Term[V,F], qpat: Term[Unit,(F,Int)]):
      Boolean =
    (term,qpat) match {
      case (_,Var(())) => true
      case (Fun(f1,args),Fun((f2,_),qpats))
          if f1 == f2 && args.length == qpats.length =>
        args.view.zip(qpats).map {
          case (arg,qpat2) => termMatchesQPat(arg,qpat2)
        }.foldLeft(true) { _ && _ }
      case _ =>
        false
    }

  private def matchesArgs[V,F,A](
    args: List[Term[V,F]],
    net:TermNetImpl[F,A]): List[A] = {
    (args,net) match {
      case (List(),Result(xs)) => xs
      case (arg::args,Stem(pat,net2)) =>
        if (termMatchesQPat(arg,pat))
          matchesArgs(args,net2)
        else List()
      case (arg::args,Branch(vnet,fmap)) =>
        val vresults = vnet.map(matchesArgs(args,_)).getOrElse(List())
        val fresults = arg match {
          case Fun(f,fargs) =>
            fmap.get((f,fargs.length))
              .map(matchesArgs(fargs++args,_)).getOrElse(List())
          case _ => List()
        }
        vresults++fresults
      case _ => throw new Error("Bug: matches")
    }
  }

  private def qTermMatchesPat[V,F](
    θ: QSubst[V,F],
    qtm:Term[Unit,(F,Int)],
    pat:Term[V,F]):
      Option[QSubst[V,F]] = {
    (qtm,pat) match {
      case (_,(Var(v))) =>
        θ.lift(v) match {
          case None       => Some(θ + (v → qtm))
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

  // This code is simpler than Hurd's, but is probably very suboptimal. Note
  // that we retrieve all possible nextTerms and *then* check whether each is
  // equal to the variable bound to v in θ. In case of unequal terms, Hurd's
  // solution is more opportunistic, not returning nextTerms as soon as they
  // are known to be unequal to the sought term. (see foldEqualTerms).
  private def matchedArgs[V,F,A](
    θ: QSubst[V,F],
    pats: List[Term[V,F]],
    net:TermNetImpl[F,A]): List[(QSubst[V,F],List[A])] = {
    (pats,net) match {
      case (List(),Result(xs)) => List((θ,xs))
      case (Var(v)::pats,net) => {
        val boundTo = θ.lift(v)
          (for ((next,net2) <- nextArg(net))
          yield boundTo match {
            case None => matchedArgs(θ + (v → next), pats, net2)
            case Some(tm) if next == tm => matchedArgs(θ, pats, net2)
            case _ => List()
          }).flatten
      }
      case (pat::pats,Stem(arg,net2)) =>
        qTermMatchesPat(θ,arg,pat).map {
          θ2 => matchedArgs(θ2,pats,net2)
        }.getOrElse(List())
      case (pat::pats,Branch(vnet,fnet)) =>
        val vmatches = vnet match {
          case None => List()
          case Some(vnet) =>
            qTermMatchesPat(θ,Var[Unit,(F,Int)](()),pat).map {
              case vθ => matchedArgs(vθ,pats,vnet)
            }.getOrElse(List())
        }
        val fmatches = pat match {
          case Fun(f,fargs) =>
            fnet.lift((f,fargs.length)).map {
              fnet => matchedArgs(θ,fargs++pats,fnet)
            }.getOrElse(List())
          case _ => List()
        }
        vmatches ++ fmatches
      case _ => throw new Error("Bug: matched")
    }
  }

  /** A trie-like map from quotiented terms (all variables are made distinct) to
    * sets of values.
    *
    * @tparam F the alphabet from which functor names are drawn
    * @tparam A the type of values
    */
  case class TermNet[F,A] private[Nets] (termNet: TermNetImpl[F,A], theSize: Int) {

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
          val vnet2 = vnet >>= (filterNet(pred,_) match {
            case Empty() => None
            case vnet    => vnet.some
          })
          val fnet2 =
            fnet.mapValues(filterNet(pred, _))
              .filter {
              case (_,Empty()) => false
              case _           => true
            }
          if (fnet2.isEmpty) {
            vnet2 match {
              case None          => Empty()
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

    /** Insert/append a new value into the map for a given pattern. All variables in
      * the pattern will be (effectively) renamed so that all are unique. */
    def insert[V](pat: Term[V,F],x: A): TermNet[F,A] = {
      new TermNet(
        insertNet(Result(List(x)),List(qTerm(pat)),this.termNet),
        this.size + 1)
    }

    /** Return all values where tm matches a key-pattern. */
    def matches[V](tm: Term[V,F]): List[A] = {
      this.termNet match {
        case Empty() => List()
        case net     => matchesArgs(List(tm),net)
      }
    }

    /** Return all values where the key matches the given pattern. */
    def matched[V](pat: Term[V,F]): List[A] = {
      this.termNet match {
        case Empty() => List()
        case net     =>
          matchedArgs(Map(), List(pat), net).map(_._2).flatten
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

    /** Return all values where tm can be unified with the key. */
    def unifies[V](tm: Term[V,F]): List[A] =
      unifiesArgs(Map(),List(tm),this.termNet)
  }

  /** A factor for trie-like maps from atoms to values.
    *
    * @tparam F the alphabet from which functor names are drawn
    * @tparam P the alphabet from which predicate names are drawn
    * @tparam A the type of values
    *
    * @param eqFunctor Internally, an atom net is just a term net, so we need a
    * functor to represent equality.
  */
  case class AtomNet[F,P,A] private (
    predNets: Map[(P,Int),TermNet[F,A]],
    eqlNet: TermNet[F,A]) {

    def insert[V](atm:Atom[V,F,P], x:A) = {
      atm match {
        case Pred(p,args) =>
          val len = args.length
          val newPredNet =
            predNets.get((p,len)) match {
              case Some(TermNet(net,size)) =>
                new TermNet(insertNet[F,A](
                  Result(List(x)),
                  args.map(qTerm(_)),
                  net),size+1)
              case None =>
                new TermNet(insertNet[F,A](
                  Result(List(x)),
                  args.map(qTerm(_)),
                  Empty()),
                  1)
            }
          new AtomNet(this.predNets + ((p,len) → newPredNet), this.eqlNet)
        case Eql(l,r) =>
          val TermNet(net,size) = eqlNet
          val newEqlNet = new TermNet(insertNet[F,A](
            Result(List(x)),
            List(qTerm(l),qTerm(r)),
            net),
            size+1)
          new AtomNet(this.predNets, newEqlNet)
      }
    }

    def this() {
      this(Map(), TermNet(Empty(),0))
    }

    def filter(pred: A=>Boolean) =
      new AtomNet(
        this.predNets.map { case (p,net) => (p,net.filter(pred)) }
          .filter { case (_,net) => net.size > 0 },
        this.eqlNet.filter(pred))

    def unifies[V](atm: Atom[V,F,P]) =
      atm match {
        case Pred(p,args) =>
          this.predNets.get((p,args.length)).map {
            case TermNet(net,_) => unifiesArgs(Map(), args, net)
          }.getOrElse(List[A]())
        case Eql(l,r) =>
          val TermNet(net,_) = this.eqlNet
          net match {
            case Empty() => List()
            case _       => unifiesArgs(Map(), List(l,r), net)
          }
      }

    def matches[V](atm: Atom[V,F,P]) =
      atm match {
        case Pred(p,args) =>
          this.predNets.get((p,args.length)).map {
            case TermNet(net,_) => matchesArgs(args, net)
          }.getOrElse(List[A]())
        case Eql(l,r) =>
          val TermNet(net,_) = this.eqlNet
          net match {
            case Empty() => List()
            case _       => matchesArgs(List(l,r), net)
          }
      }

    def matched[V](atm: Atom[V,F,P]) =
      atm match {
        case Pred(p,args) =>
          this.predNets.get((p,args.length)).map {
            case TermNet(net,_) => matchedArgs(Map(), args, net)
          }.getOrElse(List[A]())
        case Eql(l,r) =>
          val TermNet(net,_) = this.eqlNet
          net match {
            case Empty() => List()
            case _       => matchedArgs(Map(), List(l,r), net)
          }
      }

    def size =
      this.predNets.foldLeft(0) { case (n,(_,net)) => n + net.size } +
        eqlNet.size

    def isEmpty = this.size == 0
  }

  /** A factor for trie-like maps from literals to values.
    *
    * @tparam F the alphabet from which functor names are drawn
    * @tparam P the alphabet from which predicate names are drawn
    * @tparam A the type of values
    *
  */
  case class LiteralNet[F,P,A] private
    (posNet: AtomNet[F,P,A], negNet: AtomNet[F,P,A]) {
    /** Create an empty map. */
    def this() {
      this(new AtomNet(), new AtomNet())
    }

    def insert[V](lit:Literal[V,F,P], x:A) =
      if (lit.isPositive)
        new LiteralNet(this.posNet.insert(lit.atom,x), this.negNet)
      else
        new LiteralNet(this.posNet, this.negNet.insert(lit.atom,x))

    def filter(pred: A=>Boolean) =
      new LiteralNet(this.posNet.filter(pred), this.negNet.filter(pred))

    def unifies[V](lit: Literal[V,F,P]) =
      if (lit.isPositive)
        this.posNet.unifies(lit.atom)
      else
        this.negNet.unifies(lit.atom)

    def matches[V](lit: Literal[V,F,P]) =
      if (lit.isPositive)
        this.posNet.matches(lit.atom)
      else
        this.negNet.matches(lit.atom)

    def matched[V](lit: Literal[V,F,P]) =
      if (lit.isPositive)
        this.posNet.matches(lit.atom)
      else
        this.negNet.matches(lit.atom)

    def size    = this.posNet.size + this.negNet.size
    def isEmpty = this.size == 0
  }
}
