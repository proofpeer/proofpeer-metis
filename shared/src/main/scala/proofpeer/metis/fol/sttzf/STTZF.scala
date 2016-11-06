package proofpeer.metis.fol.sttzf

import org.ensime.sexp.SexpParser
import org.ensime.sexp.{ Sexp, SexpCons, SexpSymbol, SexpNumber,
  SexpCompactPrinter => SexpPrinter }

import proofpeer.metis.Sexp. { SexpFn, SexpOfFn, functorOfSexp, FOLSexpPrinter }
import proofpeer.metis. { Sexp => _, Pred => _, _ }
import proofpeer.metis.fol._
import proofpeer.metis.fol.Sexp._

import proofpeer.metis.fol.FOL.Instances._

import scalaz._
import Scalaz._

object ZFProver {
  type FOLSexpFn = FOL[SexpFn,SexpFn,SexpFn,FOL.Neg,FOL.Binder]

  def folOfString(str: String): FOLSexpFn =
    folOfSexp(SexpParser.parse(str),true)

  abstract sealed class Theorem {
    val definitions: SexpFn ==>> FOLSexpFn
    val thm: FOLSexpFn
  }
  val choice = new Theorem {
    override val thm =
      folOfString("(-> (vin x A) (vin ((Member) A) A))")
    override val definitions = IMap.empty[SexpFn,FOLSexpFn]
  }
  val extensionality = new Theorem {
    override val thm =
      folOfString("(<-> (= A B) (! x (<-> (vin x A) (vin x B))))")
    override val definitions = IMap.empty[SexpFn,FOLSexpFn]
  }
  val upair = new Theorem {
    override val thm =
      folOfString("(ex P (! x (<-> (vin x P) (or (= x a) (= x b)))))")
    override val definitions = IMap.empty[SexpFn,FOLSexpFn]
  }
  val fUnion = new Theorem {
    override val thm =
      folOfString("(ex U (! x (<-> (vin x U) (or (vin x A) (vin x B)))))")
    override val definitions = IMap.empty[SexpFn,FOLSexpFn]
  }
  val powerset = new Theorem {
    override val thm =
      folOfString("(ex P (! X (<-> (vin X P) (! x (-> (vin x X) (vin x A))))))")
    override val definitions = IMap.empty[SexpFn,FOLSexpFn]
  }
  def inst(theorem: Theorem, f: SexpFn ==>> Term[SexpFn,SexpFn]) = new Theorem {
    override val thm = theorem.thm.inst(v => f.lookup(v).getOrElse(Var(v)))
    override val definitions = theorem.definitions
  }
  def separationInst(x: SexpFn, body: FOLSexpFn): Theorem = {
    val s = -\/("Sub")
    def v(fn: SexpFn): Term[SexpFn,SexpFn] = Var[SexpFn,SexpFn](fn)
    val avoids = body.frees.insert(x)
    if (avoids.contains(s))
      throw new RuntimeException("Sub cannot be free in body")
    else {
      val l = Pred(-\/("vin"):SexpFn,List(v(x),v(s)))
      val r =
        And(Pred(-\/("vin"):SexpFn,List(v(x),v(-\/("Super")))),
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
        override val definitions = IMap.empty[SexpFn,FOLSexpFn]
      }}
    }
  def combineDefinitions(defnss: List[IMap[SexpFn,FOLSexpFn]]) = {
    defnss.foldLeft(IMap.empty[SexpFn,ISet[FOLSexpFn]]) {
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
  trait SexpTPTPPrinter[V] extends TPTPPrinter[
    V,
    \/[Fresh[SexpFn],SexpFn],
    SexpFn] {

    override def orderF = implicitly[Order[\/[Fresh[SexpFn],SexpFn]]]
    override def orderP = implicitly[Order[SexpFn]]

    def tptpOfSexpFn(sym: SexpFn): Option[Cord] = sym match {
      case -\/(sym) if sym.forall(_.isLetter) => Some(sym)
      case _ => None
    }

    def tptpOfVAux(v: \/[SexpFn,Fresh[SexpFn]]): Option[Cord] =
      (v match {
        case -\/(sym) => tptpOfSexpFn(sym)
        case \/-(fsym) => tptpOfSexpFn(fsym.origin).map { _ ++ fsym.get.show }
      })

    override def tptpOfF(v: \/[Fresh[SexpFn],SexpFn]): Option[Cord] =
      (v match {
        case -\/(fsym) => tptpOfSexpFn(fsym.origin).map { _ ++ fsym.get.show }
        case \/-(sym) => tptpOfSexpFn(sym)
      })
    override def tptpOfP(p: SexpFn): Option[Cord] = tptpOfSexpFn(p)
  }

  def SexpTPTPPrinter = new SexpTPTPPrinter[\/[SexpFn,Fresh[SexpFn]]] {
    override def orderV = implicitly[Order[\/[SexpFn,Fresh[SexpFn]]]]
    override def tptpOfV(v: \/[SexpFn,Fresh[SexpFn]]): Option[Cord] =
      tptpOfVAux(v)
  }

  def SexpResTPTPPrinter = new SexpTPTPPrinter[(\/[SexpFn,Fresh[SexpFn]],Int)] {
    override def orderV = implicitly[Order[(\/[SexpFn,Fresh[SexpFn]],Int)]]
    override def tptpOfV(v: (\/[SexpFn,Fresh[SexpFn]],Int)): Option[Cord] = {
      val (v_,idx) = v
      super.tptpOfVAux(v_).map { _ |+| "_" |+| idx.show }
    }
  }

  def cnfSexp(fol: FOLSexpFn) = {
    val (bnds,mat,_) = Matrix.quantPull(FOL.toNNF(fol))
    val mat2 = Matrix.skolemize(bnds, mat)
    CNF.cnf(mat2,-\/("="))
  }

  val icl = new IdentClause[
    \/[SexpFn,Fresh[SexpFn]],
    \/[Fresh[SexpFn],SexpFn],
    SexpFn]
  abstract class TheoremProof {
    val conjecture: FOLSexpFn
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
    theConjecture: FOLSexpFn,
    lemmas: List[Theorem],
    defns: List[SexpFn ==>> FOLSexpFn]): TheoremProof = {
    val all_defns = combineDefinitions(lemmas.map(_.definitions) ++ defns)
    new TheoremProof {
      val conjecture = theConjecture
      val resolution = icl.resolution(
        (lemmas.map { _.thm } ++
          List(Unary(FOL.Neg(),conjecture))).flatMap { cnfSexp(_).toList },
        SexpResTPTPPrinter)
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
            SexpTPTPPrinter.tptpOfClause(
              icl.fromIdentClause(Clause.trimap(cl)(_._1, i => i, i => i))) |+|
            Cord(").")
        }
      }
    }
  }

  def define(theorem: Theorem, name: String): Option[Theorem] = {
    val fol = theorem.thm
    val nameSym : SexpFn = name.left[Sexp]
    def strip(args: List[Term[SexpFn,SexpFn]])(fol: FOLSexpFn):
        Option[(SexpFn, FOLSexpFn)] =
      fol match {
        case Bnding(FOL.Exists, f, body) =>
          val args2 = args.reverse
          (f,body.inst {
            case f2 =>
              if (f === f2)
                Fun(nameSym,args2)
              else Var(f2)
          }).some
        case Bnding(FOL.All, v, p) => strip(Var[SexpFn,SexpFn](v)::args)(p)
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
