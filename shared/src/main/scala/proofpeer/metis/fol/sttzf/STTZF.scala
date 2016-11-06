package proofpeer.metis.fol.sttzf

import org.ensime.sexp.SexpParser
import org.ensime.sexp.{ Sexp, SexpCons, SexpSymbol, SexpNumber,
  SexpCompactPrinter => SexpPrinter }

import proofpeer.metis.SExpr. { SExprFn, SExprOfFn, functorOfSExpr, FOLSExprPrinter }
import proofpeer.metis. { SExpr => _, Pred => _, _ }
import proofpeer.metis.fol._
import proofpeer.metis.fol.SExpr._

import proofpeer.metis.fol.FOL.Instances._

import scalaz._
import Scalaz._

object ZFProver {
  type FOLSExprFn = FOL[SExprFn,SExprFn,SExprFn,FOL.Neg,FOL.Binder]

  def folOfString(str: String): FOLSExprFn =
    folOfSExpr(SexpParser.parse(str),true)

  abstract sealed class Theorem {
    val definitions: SExprFn ==>> FOLSExprFn
    val thm: FOLSExprFn
  }
  val choice = new Theorem {
    override val thm =
      folOfString("(-> (vin x A) (vin ((Member) A) A))")
    override val definitions = IMap.empty[SExprFn,FOLSExprFn]
  }
  val extensionality = new Theorem {
    override val thm =
      folOfString("(<-> (= A B) (! x (<-> (vin x A) (vin x B))))")
    override val definitions = IMap.empty[SExprFn,FOLSExprFn]
  }
  val upair = new Theorem {
    override val thm =
      folOfString("(ex P (! x (<-> (vin x P) (or (= x a) (= x b)))))")
    override val definitions = IMap.empty[SExprFn,FOLSExprFn]
  }
  val fUnion = new Theorem {
    override val thm =
      folOfString("(ex U (! x (<-> (vin x U) (or (vin x A) (vin x B)))))")
    override val definitions = IMap.empty[SExprFn,FOLSExprFn]
  }
  val powerset = new Theorem {
    override val thm =
      folOfString("(ex P (! X (<-> (vin X P) (! x (-> (vin x X) (vin x A))))))")
    override val definitions = IMap.empty[SExprFn,FOLSExprFn]
  }
  def inst(theorem: Theorem, f: SExprFn ==>> Term[SExprFn,SExprFn]) = new Theorem {
    override val thm = theorem.thm.inst(v => f.lookup(v).getOrElse(Var(v)))
    override val definitions = theorem.definitions
  }
  def separationInst(x: SExprFn, body: FOLSExprFn): Theorem = {
    val s = -\/("Sub")
    def v(fn: SExprFn): Term[SExprFn,SExprFn] = Var[SExprFn,SExprFn](fn)
    val avoids = body.frees.insert(x)
    if (avoids.contains(s))
      throw new RuntimeException("Sub cannot be free in body")
    else {
      val l = Pred(-\/("vin"):SExprFn,List(v(x),v(s)))
      val r =
        And(Pred(-\/("vin"):SExprFn,List(v(x),v(-\/("Super")))),
          body)
      val ax =
        Bnding(
          FOL.Exists,
          -\/("Sub"),
          Bnding(
            FOL.All,
            x,
            And(folOfImplies(l,r), folOfImplies(r,l))))
      new Theorem {
        override val thm = ax
        override val definitions = IMap.empty[SExprFn,FOLSExprFn]
      }}
    }
  def combineDefinitions(defnss: List[IMap[SExprFn,FOLSExprFn]]) = {
    defnss.foldLeft(IMap.empty[SExprFn,ISet[FOLSExprFn]]) {
      case (defn1, defn2) =>
        IMap.mergeWithKey(defn1,defn2) {
          case (_,rhss,rhs) => Some(rhss.insert(rhs))
        }(m => m, _.map { ISet.singleton(_) })
    }.mapWithKey {
      case (lhs,rhss) =>
        rhss.findMax match {
          case None =>
            throw new RuntimeException("Conflicting definitions for " + lhs)
          case Some(rhs) => rhs
        }
      }
  }

  import FOL.Fresh
  trait SExprTPTPPrinter[V] extends TPTPPrinter[
    V,
    \/[Fresh[SExprFn],SExprFn],
    SExprFn] {

    override def orderF = implicitly[Order[\/[Fresh[SExprFn],SExprFn]]]
    override def orderP = implicitly[Order[SExprFn]]

    def tptpOfSExprFn(sym: SExprFn): Option[Cord] = sym match {
      case -\/(sym) if sym.forall(_.isLetter) => Some(sym)
      case _ => None
    }

    def tptpOfVAux(v: \/[SExprFn,Fresh[SExprFn]]): Option[Cord] =
      (v match {
        case -\/(sym) => tptpOfSExprFn(sym)
        case \/-(fsym) => tptpOfSExprFn(fsym.origin).map { _ ++ fsym.get.show }
      })

    override def tptpOfF(v: \/[Fresh[SExprFn],SExprFn]): Option[Cord] =
      (v match {
        case -\/(fsym) => tptpOfSExprFn(fsym.origin).map { _ ++ fsym.get.show }
        case \/-(sym) => tptpOfSExprFn(sym)
      })
    override def tptpOfP(p: SExprFn): Option[Cord] = tptpOfSExprFn(p)
  }

  def SExprTPTPPrinter = new SExprTPTPPrinter[\/[SExprFn,Fresh[SExprFn]]] {
    override def orderV = implicitly[Order[\/[SExprFn,Fresh[SExprFn]]]]
    override def tptpOfV(v: \/[SExprFn,Fresh[SExprFn]]): Option[Cord] =
      tptpOfVAux(v)
  }

  def SExprResTPTPPrinter = new SExprTPTPPrinter[(\/[SExprFn,Fresh[SExprFn]],Int)] {
    override def orderV = implicitly[Order[(\/[SExprFn,Fresh[SExprFn]],Int)]]
    override def tptpOfV(v: (\/[SExprFn,Fresh[SExprFn]],Int)): Option[Cord] = {
      val (v_,idx) = v
      super.tptpOfVAux(v_).map { _ |+| "_" |+| idx.show }
    }
  }

  def cnfSExpr(fol: FOLSExprFn) = {
    val (bnds,mat,_) = Matrix.quantPull(FOL.toNNF(fol))
    val mat2 = Matrix.skolemize(bnds, mat)
    CNF.cnf(mat2,-\/("="))
  }

  val icl = new IdentClause[
    \/[SExprFn,Fresh[SExprFn]],
    \/[Fresh[SExprFn],SExprFn],
    SExprFn]
  abstract class TheoremProof {
    val conjecture: FOLSExprFn
    val resolution: Resolution[
      (icl.Id,icl.Id),
      icl.Id,
      icl.Id,
      icl.Id,
      icl.ResolutionBasis.kernel.type]
    def thm: Option[Theorem]
    def toTPTP: List[Cord]
  }

  def prove(
    theConjecture: FOLSExprFn,
    lemmas: List[Theorem],
    defns: List[SExprFn ==>> FOLSExprFn]): TheoremProof = {
    val all_defns = combineDefinitions(lemmas.map(_.definitions) ++ defns)
    new TheoremProof {
      val conjecture = theConjecture
      val resolution = icl.resolution(
        (lemmas.map { _.thm } ++
          List(Unary(FOL.Neg(),conjecture))).flatMap { cnfSExpr(_).toList },
        SExprResTPTPPrinter)
      val thms = resolution.distance_nextThms.takeWhile(_.isDefined).flatten
      def thm =
        thms.map { _._2 }.filter { _.isContradiction }.headOption.map { _ =>
          new Theorem {
            override val thm = conjecture
            override val definitions = all_defns
          }
        }
      override def toTPTP = {
        resolution
          .initClauses
          .zipWithIndex.map { case (cl,i) =>
            Cord("cnf(ax") |+| i.show |+|
            Cord(",axiom,") |+|
            SExprTPTPPrinter.tptpOfClause(
              icl.fromIdentClause(Clause.trimap(cl)(_._1, i => i, i => i))) |+|
            Cord(").")
        }
      }
    }
  }

  def define(theorem: Theorem, name: String): Option[Theorem] = {
    val fol = theorem.thm
    val nameSym : SExprFn = name.left[Sexp]
    def strip(args: List[Term[SExprFn,SExprFn]])(fol: FOLSExprFn):
        Option[(SExprFn, FOLSExprFn)] =
      fol match {
        case Bnding(FOL.Exists, f, body) =>
          val args2 = args.reverse
          (f,body.inst {
            case f2 =>
              if (f === f2)
                Fun(nameSym,args2)
              else Var(f2)
          }).some
        case Bnding(FOL.All, v, p) => strip(Var[SExprFn,SExprFn](v)::args)(p)
        case _ => None
      }
    strip(List())(fol).map {
      case (f, body) =>
        new Theorem {
          val definitions = IMap.singleton(nameSym,body)
          val thm = body
        }
    }
  }
}
