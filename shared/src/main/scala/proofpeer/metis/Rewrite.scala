// package proofpeer.metis

// import proofpeer.metis.util.RichCollectionInstances._
// import scala.language.implicitConversions
// import scala.collection.immutable._
// import scalaz._
// import Scalaz._

// /** KBO-based rewriting system as implemented in METIS. */
// case class METISRewriting[V:Order,F:Order,P,K<:Kernel[V,F,P]](kernel: K)(
//   implicit ordFun: Order[Fun[V,F]]) {

//   import ClauseInstances._
//   val kbo = KnuthBendix.kbo[V,F]

//   case class Equation(
//     l: Term[V,F],
//     r: Term[V,F],
//     orientation: Orientation,
//     eql: kernel.Thm)

//   object Equation {
//     def ofThm(eql: kernel.Thm): Option[Equation] =
//       for (
//         Literal(true,Eql(l,r)) <- eql.clause.lits.singleton;
//         ort <- kbo.tryCompare(l,r) match {
//           case Some(Ordering.LT) => Some(Directed(RightToLeft()))
//           case Some(Ordering.GT) => Some(Directed(LeftToRight()))
//           case Some(Ordering.EQ) => None
//           case None              => Some(Bidirectional())
//         })
//         yield new Equation(l,r,ort,eql)
//   }

//   type Conv = Term[V,F] => Option[(Term[V,F],kernel.Thm)]

//   case class Rewrite(eqn: Equation, direction: Direction) {
//     val redex = direction match {
//       case LeftToRight() => eqn.l
//       case RightToLeft() => eqn.r
//     }
//     val redux = direction match {
//       case LeftToRight() => eqn.r
//       case RightToLeft() => eqn.l
//     }
//     def symEql = {
//       (for (
//         lit <- eqn.eql.clause.lits.singleton;
//         thm <- kernel.resolve(lit,eqn.eql,kernel.sym(eqn.l,eqn.r)))
//       yield thm).getOrBug("Should always be able to flip an equation.")
//     }

//     def directedEql =
//       direction match {
//         case LeftToRight() => eqn.eql
//         case RightToLeft() => symEql
//       }

//     val conv: Conv = tm =>
//     for (
//       θ <- redex.patMatch(Subst.empty,tm).headOption;
//       // Skipping normalisation
//       redex_ = redex.subst(θ);
//       redux_ = redux.subst(θ);
//       // TODO: I don't think this is Hurd's logic. In the case of undirected
//       // rules, he always converts. But as of now, weakening this leads to infinite
//       // loops.
//       if (kbo.tryCompare(redex_,redux_) === Some(Ordering.GT)))
//     yield (redux_,directedEql.subst(θ))
//   }

//   object Rewrite {
//     def ofEquation(eqn: Equation): List[Rewrite] =
//       eqn.orientation match {
//         case Directed(dir) =>
//           Rewrite(eqn,dir).point[List]
//         case Bidirectional() =>
//           List(
//             Rewrite(eqn,LeftToRight()),
//             Rewrite(eqn,RightToLeft()))
//       }
//   }

//   // Rewrite directions
//   abstract sealed class Direction
//   case class LeftToRight() extends Direction
//   case class RightToLeft() extends Direction

//   abstract sealed class Orientation
//   case class Directed(dir:Direction) extends Orientation
//   case class Bidirectional() extends Orientation

//   // Cursors into terms in an equation.
//   abstract sealed class Cursor {
//     private def followTermPath(tm: Term[V,F], path: Vector[Int]):
//         Option[Term[V,F]] = {
//       (tm,path) match {
//         case (_,Vector()) => Some(tm)
//         case (Fun(_,args),n +: path_) =>
//           args.lift(n) >>= (followTermPath(_,path_))
//         case _ => None
//       }
//     }

//     def followPath(eqn: Equation) = this match {
//       case LHSCursor(c) => followTermPath(eqn.l,c.path)
//       case RHSCursor(c) => followTermPath(eqn.r,c.path)
//     }
//   }
//   case class LHSCursor(cursor: Term.TermCursor[V,F]) extends Cursor
//   case class RHSCursor(cursor: Term.TermCursor[V,F]) extends Cursor

//   case class Rewriter private (
//     known:    Map[Int,Equation],
//     redexes:  Nets.TermNet[F,(Int,Rewrite)],
//     subterms: Nets.TermNet[F,(Int,Cursor)],
//     waiting:  ISet[Int]) {

//     def this() =
//       this(
//         Map[Int,Equation](),
//         new Nets.TermNet[F,(Int,Rewrite)],
//         new Nets.TermNet[F,(Int,Cursor)],
//         ISet[Int]())

//     def isKnown(id: Int) = known.contains(id)

//     def isReduced = waiting.isEmpty

//     def add(id: Int, eqn: Equation) = {
//       if (known.contains(id))
//         this
//       else {
//         val rewrs = Rewrite.ofEquation(eqn)
//         val known_   = known + (id → eqn);
//         val redexes_ = rewrs.foldLeft(redexes) {
//           case (r,rewr) => r.insert(rewr.redex,(id,rewr))
//         }
//         val waiting_ = waiting + id
//         new Rewriter(known_,redexes_,subterms,waiting_)
//       }
//     }

//     /** Rewriting conversion. The first applicable rewrite in the Rewriter is used.
//       * @param id An identifier for the theorem we are supposed to be converting:
//       *           Used to check that a rewrite rule is not rewriting itself.
//       */
//     def rewr(id: Int): Conv = tm => {
//       val thms = redexes.matches(tm).view.map {
//         case (rewriteId,rewr) if id != rewriteId => rewr.conv(tm)
//         case _ => None
//       }
//       thms.find(_.isDefined).flatten
//     }

//     type NeqConvMap = Map[Literal[V,F,P],Conv]

//     // Send kbo-comparable literals to a conversion which sends the larger to the
//     // smaller.
//     def mkNeqConv(l: Term[V,F], r: Term[V,F]) = {
//       kbo.tryCompare(l,r).flatMap {
//         case Ordering.GT =>
//           Some { term:Term[V,F] =>
//             if (term == l)
//               Some(r,kernel.assume(Literal(true,Eql(l,r))))
//             else
//               None
//           }
//         case Ordering.LT =>
//           Some { term:Term[V,F] =>
//             if (term == r)
//               Some(l,kernel.sym(l,r))
//             else None
//           }
//         case Ordering.EQ => None
//       }
//     }

//     def mkNeqConvMap(lits: ISet[Literal[V,F,P]]) = {
//       lits.foldLeft((Map[Literal[V,F,P],Conv](),Set[Literal[V,F,P]]())) {
//         case ((mapAcc,litsAcc),lit@NeqLit(l,r)) => mkNeqConv(l,r) match {
//           case Some(conv) => (mapAcc + (lit → conv), litsAcc)
//           case None       => (mapAcc, litsAcc + lit)
//         }
//         case ((mapAcc,litsAcc),lit) => (mapAcc, litsAcc + lit)
//       }
//     }

//     def orElseC(conv1: Conv, conv2: Conv): Conv =
//       tm => conv1(tm).orElse(conv2(tm))

//     def collectFirstOpt[A,B](xs: TraversableOnce[A])(f: A => Option[B]):
//         Option[B] = {
//       for (x <- xs)
//         f(x) match {
//           case None  =>
//           case y     => return y
//         }
//       None
//     }

//     // Apply the first successful conversion in the map
//     def neqConvToConv(map: NeqConvMap) : Conv =
//       tm => collectFirstOpt(map) { case (_,conv) => conv(tm) }

//     def mkNeqRule(id: Int, map: NeqConvMap)(lit: Literal[V,F,P]) = {
//       val conv = orElseC(neqConvToConv(map),rewr(id))
//       kernel.repeatTopDownConvRule(lit,conv)
//     }

//     // Given a clause, interreduce all negated (conditional) equalities with the
//     // main rewrites, and use the final interreduced set of rewrites against all
//     // literals in the clause.
//     def interRewriteNeqs(thm: kernel.Thm, id: Int) = {

//       val (map,lits) = mkNeqConvMap(thm.clause)

//       def interRewrite1(
//         acc: (NeqConvMap, ISet[Literal[V,F,P]], kernel.Thm, Boolean),
//         key: Literal[V,F,P]) = {
//         val (map, lits, thm, changed) = acc
//         val newMap = map - key

//         val newAcc =
//           for (
//             (litConvThm,newLit) <- mkNeqRule(id,newMap)(key);
//             if newLit != key;
//             newThm = kernel.resolve(key,thm,litConvThm).get)
//           yield
//             newLit match {
//               case NeqLit(l,r) =>
//                 mkNeqConv(l,r) match {
//                   case Some(conv) =>
//                     (newMap + (newLit → conv), lits, newThm, true)
//                   case None => (newMap, lits + key, newThm, changed)
//                 }
//               case _ => (newMap, lits + key, thm, changed)
//             }

//         newAcc.getOrElse(acc)
//       }

//       def interRewrite(
//         map: NeqConvMap,
//         lits: ISet[Literal[V,F,P]],
//         thm: kernel.Thm):
//           (NeqConvMap, ISet[Literal[V,F,P]], kernel.Thm) = {
//         val (newMap, newLits, newThm, changed) =
//           map.keys.foldLeft((map, lits, thm, false))(interRewrite1)
//         if (changed)
//           interRewrite(newMap, newLits, newThm)
//         else (map,lits,newThm)
//       }

//       val (newMap, newLits, newThm) = interRewrite(map, lits, thm)

//       val neqRule = mkNeqRule(id,newMap)_

//       val finalThm = lits.foldLeft(newThm) {
//         case (accThm,lit) =>
//           if (newThm.clause.contains(lit))
//             (neqRule(lit).map { case (convThm,_) =>
//               kernel.resolve(lit,accThm,convThm).getOrBug(
//                 "Invalid conversion.")
//             }).getOrElse(accThm)
//           else accThm
//       }

//       finalThm
//     }

//     def rewriteEqn(thmId: Int, eqn: Equation) = {
//       val (map, _)         = mkNeqConvMap(eqn.eql.clause)
//       val eqLit            = Literal(true,Eql[V,F,P](eqn.l,eqn.r))
//       val strongEqn        = !eqn.eql.clause.contains(eqLit)
//       // TODO: Could exploit the fact that mkNeqRule returns None to say that no
//       // conversion occurred.
//       val (eqThm,newEqLit) = mkNeqRule(thmId,map)(eqLit).getOrElse((eqn.eql,eqLit))
//       // Note: Leaving out optimisation checking whether the literal has changed
//       newEqLit match {
//         case newEqLit@Literal(true,Eql(l,r)) =>
//           val thm =
//             if (strongEqn)
//               eqn.eql
//             else if (eqThm.clause.contains(eqLit.negate))
//               kernel.resolve(eqLit, eqn.eql, eqThm).get
//             else eqThm
//           (thm,l,r)
//       }
//     }

//     def isOriented(eqn: Equation) =
//       eqn.orientation match {
//         case Bidirectional() => false
//         case _               => true
//       }

//     def pick(todo: ISet[(Int)]): Option[(Int,Equation)] =
//       todo.collectFirst(Function.unlift { id =>
//         known.get(id) match {
//           case Some(eqn) if isOriented(eqn) => Some((id,eqn))
//           case _                            => None
//       }}).orElse(
//         todo.collectFirst(Function.unlift { id =>
//           known.get(id).map { eqn => (id,eqn) }
//         }))

//     def reduce1(isNew: Boolean, thmId: Int, eqn: Equation, acc: ReduceAcc):
//         (Rewriter,ReduceAcc) = {

//       val (newEql,l,r) = rewriteEqn(thmId, eqn)

//       val isIdentical = eqn.l == l && eqn.r == r

//       // We only need to check the original orientation. If the
//       // equation has switched orientation, then we know that the
//       // redexes are different, since we always rewrite to the smaller
//       // term.
//       val sameRedexes = eqn.orientation match {
//         case Directed(LeftToRight()) => eqn.l == l
//         case Directed(RightToLeft()) => eqn.r == r
//         case _                       => isIdentical
//       }

//       val redexesAcc =
//         if (sameRedexes)
//           acc.redexesAcc
//         else acc.redexesAcc + thmId
//       val subtermsAcc =
//         if (isNew || isIdentical)
//           acc.subtermsAcc
//         else
//           acc.subtermsAcc + thmId
//       val changedAcc =
//         if (!isNew && isIdentical)
//           acc.changedAcc
//         else acc.changedAcc + thmId
//       val newEquation =
//         if (sameRedexes)
//           Some(Equation(l,r,eqn.orientation,newEql))
//         else
//           Equation.ofThm(newEql)

//       newEquation match {
//         case None =>
//           (new Rewriter(
//             this.known - thmId,
//             this.redexes,
//             this.subterms,
//             this.waiting),
//             ReduceAcc(redexesAcc, subtermsAcc, acc.todoAcc, changedAcc))
//         case Some(newEqn) =>
//           val todoAcc =
//             if (!isNew && sameRedexes)
//               acc.todoAcc
//             else findReducibles(acc.todoAcc,thmId,newEqn)
//           val newKnown =
//             if (isIdentical)
//               this.known
//             else known + (thmId → newEqn)
//           val newRedexes =
//             if (sameRedexes)
//               this.redexes
//             else addRedexes(thmId, newEqn)
//           val newSubterms =
//             if (!isNew && isIdentical)
//               this.subterms
//             else addSubterms(thmId, eqn)
//           (new Rewriter(newKnown, newRedexes, newSubterms, this.waiting),
//             ReduceAcc(redexesAcc, subtermsAcc, todoAcc, changedAcc))
//       }
//     }

//     def rebuild(acc: ReduceAcc) = {
//       val newRedexes  = this.redexes.filter {
//         case (id, _) => !acc.redexesAcc.contains(id)
//       }
//       val newSubterms = this.subterms.filter {
//         case (id,_) => !acc.subtermsAcc.contains(id)
//       }

//       val newRedexes_ = acc.redexesAcc.foldLeft(newRedexes) {
//         case (theRedexes, id) =>
//           this.known.get(id) match {
//             case None      => theRedexes
//             case Some(eqn) =>
//               Rewrite.ofEquation(eqn).foldLeft(theRedexes) {
//                 case (theRedexes_,rewr) => theRedexes_.insert(rewr.redex,(id,rewr))
//               }
//           }
//       }
//       val newSubterms_ = acc.subtermsAcc.foldLeft(newSubterms) {
//         case (theSubterms, id) =>
//           this.known.get(id) match {
//             case None      => theSubterms
//             case Some(eqn) => addSubterms(id,eqn)
//           }
//       }
//       new Rewriter(known,newRedexes_,newSubterms_,waiting)
//     }

//     def findReducibles(todo: ISet[Int], thmId: Int, eqn: Equation) = {
//       val rewrs = Rewrite.ofEquation(eqn)
//       rewrs.foldLeft(todo) {
//         case (todo,rewr) =>
//           subterms.matched(rewr.redex).foldLeft(todo) {
//             case (todo,(thmId_,cursor))
//                 if thmId != thmId_
//                 && !(todo.contains(thmId_))
//                 && isOriented(eqn) =>
//               val rewritable = for (
//                 eqn_ <- known.get(thmId_);
//                 // We're following Hurd here, getting the actual subterm by following
//                 // a path. I assume we need to do this because it's possible that
//                 // the equation has been rewritten and that the subterm is no longer
//                 // the one we originally put in the net. Otherwise, why not just
//                 // store the term without a path/cursor?
//                 subterm <- cursor.followPath(eqn_);
//                 θ <- rewr.redex.patMatch(Subst.empty,subterm).headOption;
//                 // skip normalisation of substitution
//                 Ordering.GT <- kbo.tryCompare(rewr.redex,rewr.redux.subst(θ)))
//               yield thmId_

//               todo ++ rewritable

//             case _ => todo
//           }
//       }
//     }

//     def addRedexes(thmId: Int, eqn: Equation) = {
//       val rewrs = Rewrite.ofEquation(eqn)
//       rewrs.foldLeft(redexes) {
//         case (redexes,rewr) =>
//           redexes.insert(rewr.redex,(thmId,rewr))
//       }
//     }

//     def addSubterms(thmId: Int, eqn: Equation) = {
//       val newSubterms = eqn.l.allSubterms.foldLeft(this.subterms) {
//         case (subterms,cursor) =>
//           subterms.insert(cursor.get,(thmId,LHSCursor(cursor)))
//       }
//       eqn.r.allSubterms.foldLeft(newSubterms) {
//         case (subterms,cursor) =>
//           subterms.insert(cursor.get,(thmId,RHSCursor(cursor)))
//       }
//     }

//     def reduceAcc(acc: ReduceAcc): (Rewriter, ISet[Int]) = {
//       pick(acc.todoAcc) match {
//         case Some((id,eqn)) =>
//           val (newRw, newAcc) = reduce1(false, id, eqn, ReduceAcc(
//             acc.redexesAcc, acc.subtermsAcc, acc.todoAcc - id, acc.changedAcc))
//           newRw.reduceAcc(newAcc)
//         case None =>
//           pick(this.waiting) match {
//             case Some((id,eqn)) =>
//               val newRw = new Rewriter(known, redexes, subterms, this.waiting - id)
//               val (newRw2, newAcc) =
//                 newRw.reduce1(true, id, eqn, acc)
//               newRw2.reduceAcc(newAcc)
//             case None => (rebuild(acc),acc.changedAcc)
//           }
//       }
//     }

//     def reduce = reduceAcc(ReduceAcc(Set(),Set(),Set(),Set()))
//   }

//   case class ReduceAcc(
//     redexesAcc:  ISet[Int],
//     subtermsAcc: ISet[Int],
//     todoAcc:     ISet[Int],
//     changedAcc:  ISet[Int])
// }
