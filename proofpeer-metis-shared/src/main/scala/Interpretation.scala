package proofpeer.metis

import scala.language.higherKinds
import scala.collection.immutable
import scalaz._
import Scalaz._
import util.Fun._

/** Finite/random interpretations of clauses. */
case class Interpretation[V,F,P](
  maxVals: Int,
  vals: Valuations[V]) {
  def safeExp(x:Int,n:Int) =
    if (n == 0)
      Some(1)
    else {
      iterateM(n-1, x.toLong) { acc =>
        val next = x * acc
        if (next < Integer.MAX_VALUE)
          Some(next)
        else None
      } map {_.toInt }
    }

  val maxArgs = {
    val b = vals.fin.size.toLong
    var acc = b
    while (acc < Integer.MAX_VALUE)
      acc = acc * b
    acc
  }

  case class S(seed:Long,fis:FunctorInterpretations,pis:PredicateInterpretations)
  type M[A] = State[S,A]

  type FunctorInterpretations   = RInterpretation[F,vals.fin.Fin]
  type PredicateInterpretations = RInterpretation[P,Boolean]

  case class ListMapping[Cod] (m: Map[vals.fin.ListFin,Cod]) {
    def +(argsImg: Tuple2[vals.fin.ListFin, Cod]) = {
      val (args,img) = argsImg
      ListMapping(m + (args → img))
    }
    def img(args: vals.fin.ListFin, default: M[Cod]):
        M[(ListMapping[Cod],Cod)] = {
      m.get(args) match {
        case None =>
          for (y <- default)
          yield (ListMapping(m + (args → y)), y)
        case Some(n) => (this, n).point[M]
      }
    }
  }

  object ListMapping {
    def empty[Cod] = new ListMapping[Cod](Map())
  }

  case class RInterpretation[F,Cod] (m: Map[(F,Int),ListMapping[Cod]]) {
    def interpret(f:F, args:vals.fin.ListFin, default: M[Cod]):
        M[(RInterpretation[F,Cod], Cod)] = {
      val nargs   = args.ns.length
      val mapping = m.getOrElse((f,nargs),ListMapping.empty)
      mapping.img(args,default) map { case (i,y) =>
        (RInterpretation(m + { (f,nargs) → i }), y) }
    }
    def reinterpret(f:F, args:vals.fin.ListFin, img: Cod): RInterpretation[F,Cod] = {
      val nargs   = args.ns.length
      val mapping = m.getOrElse((f,nargs),ListMapping.empty)
      RInterpretation(m + { (f,nargs) → (mapping + { args → img }) })
    }
  }

  object RInterpretation {
    def empty[F,Cod] = new RInterpretation[F,Cod](Map())
  }

  def getFIs = for (s <- get[S]) yield s.fis
  def getPIs = for (s <- get[S]) yield s.pis

  def liftRand[A](mx:MetisRNG.M[A]): M[A] =
    for (
      s         <- get[S];
      (seed2,x) = mx.run(s.seed);
      _         <- put(S(seed2,s.fis,s.pis)))
    yield x

  def interpretFunction(f:F, args:vals.fin.ListFin): M[vals.fin.Fin] = {
    for (
      s           <- get[S];
      newFI_value <- s.fis.interpret(f,args,liftRand(vals.fin.random));
      s_          <- get[S];
      _           <- put(S(s_.seed,newFI_value._1,s_.pis)))
    yield newFI_value._2
  }

  def interpretPredicate(p:P, args:vals.fin.ListFin): M[Boolean] = {
    for (
      s           <- get[S];
      newPI_value <- s.pis.interpret(p,args,liftRand(MetisRNG.nextBoolean));
      s_          <- get[S];
      _           <- put(S(s_.seed,s_.fis,newPI_value._1)))
    yield newPI_value._2
  }

  def reinterpretFunction(f:F, args:vals.fin.ListFin, img:vals.fin.Fin): M[Unit] = {
    for (
      s     <- get[S];
      newFI = s.fis.reinterpret(f,args,img);
      s_    <- get[S];
      _     <- put(S(s_.seed,newFI,s_.pis)))
    yield ()
  }

  def reinterpretPredicate(p:P, args:vals.fin.ListFin, img:Boolean): M[Unit] = {
    for (
      s     <- get[S];
      newPI = s.pis.reinterpret(p,args,img);
      s_    <- get[S];
      _     <- put(S(s_.seed,s_.fis,newPI)))
    yield ()
  }

  def interpretTerm(vl: vals.Valuation, t: Term[V,F]): M[vals.fin.Fin] = {
    t match {
      case Var(v)      => (vl.get(v).get).point[M]
      case Fun(f,args) =>
        val nargs = args.length
        if (nargs < maxArgs)
          for (
            vargs <- args.traverse(interpretTerm(vl,_));
            y     <- interpretFunction(f,vals.fin.ListFin(vargs))
          )
          yield y
        else {
          for (n <- liftRand(vals.fin.random))
          yield n
        }
    }
  }

  def interpretAtom(vl: vals.Valuation, atm: Atom[V,F,P]): M[Boolean] = {
    atm match {
      case Eql(x,y) => (interpretTerm(vl,x) |@| interpretTerm(vl,y))(_ == _)
      case Pred(p,args) =>
        val nargs = args.length
        if (nargs < maxArgs)
          for (
            vargs <- args.traverse(interpretTerm(vl,_));
            y     <- interpretPredicate(p,vals.fin.ListFin(vargs))
          )
          yield y
        else {
          for (b <- liftRand(MetisRNG.nextBoolean))
          yield b
        }
    }
  }

  def interpretLiteral(vl: vals.Valuation, lit: Literal[V,F,P]): M[Boolean] = {
    interpretAtom(vl,lit.atom) map (_ == lit.isPositive)
  }

  def interpretClause(vl: vals.Valuation, cl: Clause[V,F,P]) = {
    existsM(cl.lits.iterator)(interpretLiteral(vl,_))
  }

  def checkClause(maxChecks: Option[Int], cl: Clause[V,F,P]) = {
    def score(vl: vals.Valuation, m: Map[Boolean,Int]) = {
      interpretClause(vl, cl) map {
        b => if (b)
          m + { true → (m(true) + 1) }
        else
          m + { false → (m(false) + 1) }
      }
    }

    val fvs = cl.frees

    // Exhaustively check if fewer cases than maxChecks
    val empty = Map(
      true  → 0,
      false → 0
    )
    val suggestedChecks = maxChecks.map { n =>
      safeExp(vals.fin.size,fvs.size).filter { _ <= n }.getOrElse(n)
    }
    suggestedChecks match  {
      case None => vals.foldValuations(fvs, empty.point[M]) {
        case (mm,vl) => mm >>= (score(vl,_))
      }
      case Some(chks) => iterateM[M,Map[Boolean,Int]](chks,empty) {
        m => liftRand(vals.random(fvs)) >>= (score(_,m))
      }
    }
  }

  def splits[A](xs: List[A]) = {
    def f(pre: List[A], xs: List[A], acc: List[(List[A],A,List[A])]):
        List[(List[A],A,List[A])] = {
      xs match {
        case List() => acc
        case y::ys  => f(y::pre, ys, (pre,y,ys)::acc)
      }
    }
    f(List(), xs, List())
  }

  def valueModelTerm(term: Term[vals.fin.Fin,(F,vals.fin.Fin)]) = {
    term match {
      case Var(n)       => n
      case Fun((_,n),_) => n
    }
  }

  import vals.fin.ordFin
  def toModelTerm(term: Term[V,F], vl: vals.Valuation):
      M[Term[vals.fin.Fin,(F,vals.fin.Fin)]] = {
    term match {
      case Var(v)      =>
        val theV = Var[vals.fin.Fin,(F,vals.fin.Fin)](vl.get(v).get):
            Term[vals.fin.Fin,(F,vals.fin.Fin)]
        theV.point[M]
      case Fun(f,args) =>
        val argVals = args.traverseU(toModelTerm(_,vl))
        for (
          avs <- argVals;
          img <- interpretFunction(f,vals.fin.ListFin(avs.map(valueModelTerm(_)))))
        yield Fun((f,img),avs)
    }
  }

  sealed trait Perturbation
  case class PerturbFunction(f:F,args:List[vals.fin.Fin],img:vals.fin.Fin)
      extends Perturbation
  case class PerturbPredicate(p:P,args:List[vals.fin.Fin],img:Boolean)
      extends Perturbation

  def perturbTerm(
    term: Term[vals.fin.Fin,(F,vals.fin.Fin)],
    targets: List[vals.fin.Fin]): M[List[Perturbation]] = {
    term match {
      case Var(_) => List[Perturbation]().point[M]
      case Fun((f,_),args) =>
        val argVals = args.map(valueModelTerm)
        splits(argVals).reverse.zip(args).traverseM {
          case ((pre,v,suf),arg) =>
            val vs = vals.fin.filterM[M] { v_ =>
              if (v == v_)
                false.point[M]
              else
                interpretFunction(
                  f,
                  vals.fin.ListFin(pre.reverse:::(v_ :: suf))).
                  map { fv => targets.contains(fv) }
            }
            vs >>= (perturbTerm(arg,_))
        }.map {
          _ ++ targets.map(PerturbFunction(f,argVals,_))
        }
    }
  }

  def perturbLiteral(lit: Literal[V,F,P], valuation: vals.Valuation) = {
    lit match {
      case Literal(isPositive,Eql(x,y)) =>
        for (
          mx     <- toModelTerm(x,valuation);
          my     <- toModelTerm(y,valuation);
          perts  <- perturbTerm(mx, vals.fin.filter { _ => true }.toList);
          xv = valueModelTerm(mx);
          perts_ <- perturbTerm(my, vals.fin.filter (xv == _ == isPositive).toList))
        yield perts ++ perts_
      case Literal(isPositive,Pred(p,args)) =>
        for (
          margs   <- args.traverseU(toModelTerm(_,valuation));
          argVals = margs.map(valueModelTerm);
          perts   <- splits(argVals).reverse.zip(margs).traverseM {
            case ((pre,v,suf),arg) =>
              val vs = vals.fin.filterM[M] { v_ =>
                if (v == v_)
                  false.point[M]
                else
                  interpretPredicate(
                    p,
                    vals.fin.ListFin(pre.reverse:::(v_ :: suf))).
                    map { pv => pv == isPositive }
              }
              vs >>= (perturbTerm(arg,_))
          }.map {
            PerturbPredicate(p,argVals,isPositive) :: _
          })
          yield perts
    }
  }

  def perturbClause(cl: Clause[V,F,P], valuation: vals.Valuation) =
    cl.lits.toList.traverseM[M,Perturbation](perturbLiteral(_,valuation))

  def applyPerturbation(pert: Perturbation): M[Unit] =
    pert match {
      case PerturbFunction(f,args,img) =>
        reinterpretFunction(f,vals.fin.ListFin(args),img)
      case PerturbPredicate(p,args,img) =>
        reinterpretPredicate(p,vals.fin.ListFin(args),img)
    }

  def randomPerturbation(cl: Clause[V,F,P], valuation: vals.Valuation): M[Unit] =
    perturbClause(cl, valuation) >>= { ps =>
      if (!ps.isEmpty) {
        for (
          p  <- liftRand(MetisRNG.pickElement(ps));
          _  <- applyPerturbation(p);
          v  <- interpretClause(valuation,cl)
        )
        yield ()
      }
      else ().point[M]
    }

  def runFrom[A](state: S, m: M[A]) = {
    m.run(state)
  }

  def initState(seed: Long) = S(seed,RInterpretation.empty,RInterpretation.empty)

  def run[A](seed: Long, m: M[A]) = {
    m.run(initState(seed))
  }


  def preview[A](ms: Stream[M[A]])(state: S): Stream[(S,A)] = {
    var s = state
    ms.map { n =>
      val (state_,x) = n.run(state)
      s = state_
      (s,x)
    }

    // TODO: Why doesn't this work?
    // ms match {
    //   case Stream()   => Stream()
    //   case (n #:: ns) =>
    //     val (state_,x) = n.run(state)
    //     (state_,x) #:: preview(ns)(state_)
    // }
  }

  // Debug
  def printState(s: S, fp: F => String, pp: P => String) = {
    def outI[F,Cod](fp: F => String, i: RInterpretation[F,Cod]) = {
      for (
        ((f,arity),lm) <- i.m;
        (args,cod)     <- lm.m
      ) {
        System.out.println(fp(f) + "/" + arity + "(" +
          (args.ns.map(_.n.toString) intersperse ", ").foldLeft("") {
            case (str,xs) => str + xs
          } + ")" + " = " + cod)
      }
    }

    System.out.println(s.seed)
    outI(fp, s.fis)
    outI(pp, s.pis)
  }
}
