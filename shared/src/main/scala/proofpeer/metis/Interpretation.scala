// package proofpeer.metis

// import scala.language.higherKinds
// import scala.collection.immutable
// import scalaz._
// import Scalaz._
// import util.Fun._

// /** Finite/random interpretations of clauses.
//   *
//   * @tparam V The alphabet from which variable names are drawn
//   * @tparam F The alphabet from which functor names are drawn
//   * @tparam P The alphabet from which predicate names are drawn
//   * @param  maxImages The maximum number of concrete tuples for which to generate
//   *         interpretations for each predicate/function
// */
// case class Interpretation[V,F,P](
//   maxImages: Int,
//   valsF: Valuations[V]) {
//   def safeExp(x:Int,n:Int) =
//     if (n == 0)
//       Some(1)
//     else {
//       iterateM(n-1, x.toLong) { acc =>
//         val next = x * acc
//         if (next < Integer.MAX_VALUE)
//           Some(next)
//         else None
//       } map {_.toInt }
//     }

//   val maxArgs = {
//     val b = valsF.fin.size.toLong
//     var max = 0
//     var acc = b
//     while (acc < Integer.MAX_VALUE && acc < maxImages) {
//       max = max + 1
//       acc = acc * b
//     }
//     max
//   }

//   case class S(seed:Long,fis:FunctorInterpretations,pis:PredicateInterpretations)
//   type M[A] = State[S,A]

//   type FunctorInterpretations   = RInterpretation[F,valsF.fin.Fin]
//   type PredicateInterpretations = RInterpretation[P,Boolean]

//   case class ListMapping[Cod] (m: Map[valsF.fin.ListFin,Cod]) {
//     def +(argsImg: Tuple2[valsF.fin.ListFin, Cod]) = {
//       val (args,img) = argsImg
//       ListMapping(m + (args → img))
//     }
//     def img(args: valsF.fin.ListFin, default: M[Cod]):
//         M[(ListMapping[Cod],Cod)] = {
//       m.get(args) match {
//         case None =>
//           for (y <- default)
//           yield (ListMapping(m + (args → y)), y)
//         case Some(n) => (this, n).point[M]
//       }
//     }
//   }

//   object ListMapping {
//     def empty[Cod] = new ListMapping[Cod](Map())
//   }

//   case class RInterpretation[F,Cod] (m: Map[(F,Int),ListMapping[Cod]]) {
//     def interpret(f:F, args:valsF.fin.ListFin, default: M[Cod]):
//         M[(RInterpretation[F,Cod], Cod)] = {
//       val nargs   = args.ns.length
//       val mapping = m.getOrElse((f,nargs),ListMapping.empty)
//       mapping.img(args,default) map { case (i,y) =>
//         (RInterpretation(m + { (f,nargs) → i }), y) }
//     }
//     def reinterpret(f:F, args:valsF.fin.ListFin, img: Cod): RInterpretation[F,Cod] = {
//       val nargs   = args.ns.length
//       val mapping = m.getOrElse((f,nargs),ListMapping.empty)
//       RInterpretation(m + { (f,nargs) → (mapping + { args → img }) })
//     }
//   }

//   object RInterpretation {
//     def empty[F,Cod] = new RInterpretation[F,Cod](Map())
//   }

//   def getFIs = for (s <- get[S]) yield s.fis
//   def getPIs = for (s <- get[S]) yield s.pis

//   def liftRand[A](mx:MetisRNG.M[A]): M[A] =
//     for (
//       s         <- get[S];
//       (seed2,x) = mx.run(s.seed);
//       _         <- put(S(seed2,s.fis,s.pis)))
//     yield x

//   def interpretFunction(f:F, args:valsF.fin.ListFin): M[valsF.fin.Fin] = {
//     for (
//       s           <- get[S];
//       newFI_value <- s.fis.interpret(f,args,liftRand(valsF.fin.random));
//       s_          <- get[S];
//       _           <- put(S(s_.seed,newFI_value._1,s_.pis)))
//     yield newFI_value._2
//   }

//   def interpretPredicate(p:P, args:valsF.fin.ListFin): M[Boolean] = {
//     for (
//       s           <- get[S];
//       newPI_value <- s.pis.interpret(p,args,liftRand(MetisRNG.nextBoolean));
//       s_          <- get[S];
//       _           <- put(S(s_.seed,s_.fis,newPI_value._1)))
//     yield newPI_value._2
//   }

//   def reinterpretFunction(f:F, args:valsF.fin.ListFin, img:valsF.fin.Fin): M[Unit] = {
//     for (
//       s     <- get[S];
//       newFI = s.fis.reinterpret(f,args,img);
//       s_    <- get[S];
//       _     <- put(S(s_.seed,newFI,s_.pis)))
//     yield ()
//   }

//   def reinterpretPredicate(p:P, args:valsF.fin.ListFin, img:Boolean): M[Unit] = {
//     for (
//       s     <- get[S];
//       newPI = s.pis.reinterpret(p,args,img);
//       s_    <- get[S];
//       _     <- put(S(s_.seed,s_.fis,newPI)))
//     yield ()
//   }

//   def interpretTerm(vl: valsF.Valuation, t: Term[V,F]): M[valsF.fin.Fin] = {
//     t match {
//       case Var(v)      => (vl.get(v).get).point[M]
//       case Fun(f,args) =>
//         val nargs = args.length
//         if (nargs < maxArgs)
//           for (
//             vargs <- args.traverse(interpretTerm(vl,_));
//             y     <- interpretFunction(f,valsF.fin.ListFin(vargs))
//           )
//           yield y
//         else {
//           for (n <- liftRand(valsF.fin.random))
//           yield n
//         }
//     }
//   }

//   def interpretAtom(vl: valsF.Valuation, atm: Atom[V,F,P]): M[Boolean] = {
//     atm match {
//       case Eql(x,y) => (interpretTerm(vl,x) |@| interpretTerm(vl,y))(_ == _)
//       case Pred(p,args) =>
//         val nargs = args.length
//         if (nargs < maxArgs)
//           for (
//             vargs <- args.traverse(interpretTerm(vl,_));
//             y     <- interpretPredicate(p,valsF.fin.ListFin(vargs))
//           )
//           yield y
//         else {
//           for (b <- liftRand(MetisRNG.nextBoolean))
//           yield b
//         }
//     }
//   }

//   def interpretLiteral(vl: valsF.Valuation, lit: Literal[V,F,P]): M[Boolean] = {
//     interpretAtom(vl,lit.atom) map (_ == lit.isPositive)
//   }

//   def interpretClause(vl: valsF.Valuation, cl: Clause[V,F,P]) = {
//     existsM(cl.lits.iterator)(interpretLiteral(vl,_))
//   }

//   def checkClause(maxChecks: Option[Int], cl: Clause[V,F,P]) = {
//     def score(vl: valsF.Valuation, m: Map[Boolean,Int]) = {
//       interpretClause(vl, cl) map {
//         b => if (b)
//           m + { true → (m(true) + 1) }
//         else
//           m + { false → (m(false) + 1) }
//       }
//     }

//     val fvs = cl.frees

//     // Exhaustively check if fewer cases than maxChecks
//     val empty = Map(
//       true  → 0,
//       false → 0
//     )
//     val suggestedChecks = maxChecks.map { n =>
//       safeExp(valsF.fin.size,fvs.size).filter { _ <= n }.getOrElse(n)
//     }
//     suggestedChecks match  {
//       case None => valsF.foldValuations(fvs, empty.point[M]) {
//         case (mm,vl) => mm >>= (score(vl,_))
//       }
//       case Some(chks) => iterateM[M,Map[Boolean,Int]](chks,empty) {
//         m => liftRand(valsF.random(fvs)) >>= (score(_,m))
//       }
//     }
//   }

//   def valueModelTerm(term: Term[valsF.fin.Fin,(F,valsF.fin.Fin)]) = {
//     term match {
//       case Var(n)       => n
//       case Fun((_,n),_) => n
//     }
//   }

//   import valsF.fin.ordFin
//   def toModelTerm(term: Term[V,F], vl: valsF.Valuation):
//       M[Term[valsF.fin.Fin,(F,valsF.fin.Fin)]] = {
//     term match {
//       case Var(v)      =>
//         val theV = Var[valsF.fin.Fin,(F,valsF.fin.Fin)](vl.get(v).get):
//             Term[valsF.fin.Fin,(F,valsF.fin.Fin)]
//         theV.point[M]
//       case Fun(f,args) =>
//         val argVals = args.traverseU(toModelTerm(_,vl))
//         for (
//           avs <- argVals;
//           img <- interpretFunction(f,valsF.fin.ListFin(avs.map(valueModelTerm(_)))))
//         yield Fun((f,img),avs)
//     }
//   }

//   sealed trait Perturbation
//   case class PerturbFunction(f:F,args:List[valsF.fin.Fin],img:valsF.fin.Fin)
//       extends Perturbation
//   case class PerturbPredicate(p:P,args:List[valsF.fin.Fin],img:Boolean)
//       extends Perturbation

//   def perturbTerm(
//     term: Term[valsF.fin.Fin,(F,valsF.fin.Fin)],
//     targets: List[valsF.fin.Fin]): M[List[Perturbation]] = {
//     term match {
//       case Var(_) => List[Perturbation]().point[M]
//       case Fun((f,_),args) =>
//         val argVals = args.map(valueModelTerm)
//         util.Fun.splits(argVals).reverse.zip(args).traverseM {
//           case ((pre,v,suf),arg) =>
//             val vs = valsF.fin.filterM[M] { v_ =>
//               if (v == v_)
//                 false.point[M]
//               else
//                 interpretFunction(
//                   f,
//                   valsF.fin.ListFin(pre.reverse:::(v_ :: suf))).
//                   map { fv => targets.contains(fv) }
//             }
//             vs >>= (perturbTerm(arg,_))
//         }.map {
//           _ ++ targets.map(PerturbFunction(f,argVals,_))
//         }
//     }
//   }

//   def perturbLiteral(lit: Literal[V,F,P], valuation: valsF.Valuation) = {
//     lit match {
//       case Literal(isPositive,Eql(x,y)) =>
//         for (
//           mx     <- toModelTerm(x,valuation);
//           my     <- toModelTerm(y,valuation);
//           xv     =  valueModelTerm(mx);
//           yv     =  valueModelTerm(my);
//           xperts <- perturbTerm(mx, valsF.fin.filter
//             { v => (v == yv) == isPositive }.toList);
//           yperts <- perturbTerm(mx, valsF.fin.filter
//             { v => (v == xv) == isPositive }.toList))
//         yield xperts ++ yperts
//       case Literal(isPositive,Pred(p,args)) =>
//         for (
//           margs   <- args.traverseU(toModelTerm(_,valuation));
//           argVals = margs.map(valueModelTerm);
//           perts   <- splits(argVals).reverse.zip(margs).traverseM {
//             case ((pre,v,suf),arg) =>
//               val vs = valsF.fin.filterM[M] { v_ =>
//                 if (v == v_)
//                   false.point[M]
//                 else
//                   interpretPredicate(
//                     p,
//                     valsF.fin.ListFin(pre.reverse:::(v_ :: suf))).
//                     map { pv => pv == isPositive }
//               }
//               vs >>= (perturbTerm(arg,_))
//           }.map {
//             PerturbPredicate(p,argVals,isPositive) :: _
//           })
//           yield perts
//     }
//   }

//   def perturbClause(cl: Clause[V,F,P], valuation: valsF.Valuation) =
//     cl.lits.toList.traverseM[M,Perturbation](perturbLiteral(_,valuation))

//   def applyPerturbation(pert: Perturbation): M[Unit] =
//     pert match {
//       case PerturbFunction(f,args,img) =>
//         reinterpretFunction(f,valsF.fin.ListFin(args),img)
//       case PerturbPredicate(p,args,img) =>
//         reinterpretPredicate(p,valsF.fin.ListFin(args),img)
//     }

//   def randomPerturbation(cl: Clause[V,F,P], valuation: valsF.Valuation): M[Unit] =
//     perturbClause(cl, valuation) >>= { ps =>
//       if (!ps.isEmpty) {
//         for (
//           p  <- liftRand(MetisRNG.pickElement(ps));
//           _  <- applyPerturbation(p);
//           v  <- interpretClause(valuation,cl)
//         )
//         yield ()
//       }
//       else ().point[M]
//     }

//   def runFrom[A](state: S, m: M[A]) = {
//     m.run(state)
//   }

//   def initState(seed: Long) = S(seed,RInterpretation.empty,RInterpretation.empty)

//   def run[A](seed: Long, m: M[A]) = {
//     m.run(initState(seed))
//   }


//   def preview[A](ms: Stream[M[A]])(state: S): Stream[(S,A)] = {
//     var s = state
//     ms.map { n =>
//       val (state_,x) = n.run(state)
//       s = state_
//       (s,x)
//     }

//     // TODO: Why doesn't this work?
//     // ms match {
//     //   case Stream()   => Stream()
//     //   case (n #:: ns) =>
//     //     val (state_,x) = n.run(state)
//     //     (state_,x) #:: preview(ns)(state_)
//     // }
//   }

//   // Debug
//   def printState(s: S, fp: F => String, pp: P => String) = {
//     def outI[F,Cod](fp: F => String, i: RInterpretation[F,Cod]) = {
//       for (
//         ((f,arity),lm) <- i.m;
//         (args,cod)     <- lm.m
//       ) {
//         System.out.println(fp(f) + "/" + arity + "(" +
//           (args.ns.map(_.n.toString) intersperse ", ").foldLeft("") {
//             case (str,xs) => str + xs
//           } + ")" + " = " + cod)
//       }
//     }

//     System.out.println(s.seed)
//     outI(fp, s.fis)
//     outI(pp, s.pis)
//   }
// }
