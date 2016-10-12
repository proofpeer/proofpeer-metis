package proofpeer.metis.fol.sttzf

import org.ensime.sexp.{ SexpParser, SexpCompactPrinter => SexpPrinter }
import org.ensime.sexp.{ Sexp, SexpCons, SexpSymbol, SexpNumber }

import proofpeer.metis. { SExpr => _, Pred => _, _ }
import proofpeer.metis.fol._

import FOL.Instances._

import scalaz._
import Scalaz._

object ZFProver {
  type FOLSexp = FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]

  def folOfString(str: String):
      ValidationNel[(Sexp,String),FOLSexp] =
    SExpr.folOfSExpr(SexpParser.parse(str),true)

  object ResolutionBasis {
    implicit val ordFun = KnuthBendix.precedenceOrder[Sexp,Sexp]
    implicit val kbo = KnuthBendix.kbo[Sexp,Sexp]
    val kernel = new Kernel[Sexp, Sexp, Sexp]
    val factor = new Factor[Sexp, Sexp, Sexp]
    val litOrd = new MetisLiteralOrdering(kbo)
    val fin    = FinOrd(8)
    val vals   = Valuations[Sexp](fin)
    val interpretation = Interpretation[Sexp,Sexp,Sexp](1000,vals)
    val ithmF  = new IThmFactory[Sexp,Sexp,Sexp,Int,kernel.type](
      kernel,
      0,
      { case (n,v) => (n+1,SexpCons(v,SexpNumber(n))) },
      litOrd,
      factor)
  }

  abstract sealed class Theorem {
    val definitions: Sexp ==>> FOLSexp
    val thm: FOLSexp
  }
  abstract class ResTheorem extends Theorem {
    val resolution: Resolution[Sexp,Sexp,Sexp,Int,ResolutionBasis.kernel.type] 
  }
  val extensionality = new Theorem {
    override val Success(thm) =
      folOfString("(<-> (= A B) (! x (<-> (vin x A) (vin x B))))")
    override val definitions = IMap.empty[Sexp,FOLSexp]
  }
  val upair = new Theorem {
    override val Success(thm) =
      folOfString("(ex P (! x (<-> (vin x P) (or (= x a) (= x b)))))")
    override val definitions = IMap.empty[Sexp,FOLSexp]
  }
  val fUnion = new Theorem {
    override val Success(thm) =
      folOfString("(ex U (! x (<-> (vin x P) (or (vin x A) (vin x B)))))")
    override val definitions = IMap.empty[Sexp,FOLSexp]
  }
  val powerset = new Theorem {
    override val Success(thm) =
      folOfString("(ex P (! X (<-> ((vin) X P) (! x (-> (vin x X) (vin x A))))))")
    override val definitions = IMap.empty[Sexp,FOLSexp]
  }
  val Success(sepScheme) =
    folOfString("(ex S (! x (<-> (vin x S) (and (vin x A) (P x)))))")
  def separationInst(
    inst: Term[Sexp,Sexp] => FOLSexp) = {
    (FOL.instPred1(sepScheme) {
      case (p,x) => if (p === SexpSymbol("P")) inst(x) else Pred(p,List(x))
    }).map { ax => new Theorem {
      override val thm = ax
      override val definitions = IMap.empty[Sexp,FOLSexp]
    }}
  }
  def combineDefinitions(defnss: List[IMap[Sexp,FOLSexp]]):
      ValidationNel[Sexp,IMap[Sexp,FOLSexp]] = {
    defnss.foldLeft(IMap.empty[Sexp,ISet[FOLSexp]]) {
      case (defn1, defn2) =>
        IMap.mergeWithKey(defn1,defn2) {
          case (_,rhss,rhs) => Some(rhss.insert(rhs))
        }(_ => IMap.empty[Sexp,ISet[FOLSexp]], _ => IMap.empty[Sexp,ISet[FOLSexp]])
    }.traverseWithKey[({type G[A] = ValidationNel[Sexp,A]})#G,FOLSexp] {
      case (lhs,rhss) =>
        if (rhss.size > 1)
          lhs.failureNel
        else rhss.elems.head.success
      }
  }

  def sexpPrinter = new DefaultPrinter[Sexp,Sexp,Sexp] {
    def printV(v: Sexp) = Cord(SexpPrinter(v))
    def printF(f: Sexp) = Cord(SexpPrinter(f))
    def printP(p: Sexp) = Cord(SexpPrinter(p))
  }

  def prove(
    conjecture: FOLSexp,
    lemmas: List[Theorem]): ValidationNel[Sexp,Option[ResTheorem]] = {
    combineDefinitions(lemmas.map(_.definitions)).map { defns =>
      implicit val ordFun = ResolutionBasis.ordFun
      implicit val kbo = ResolutionBasis.kbo
      val res: Resolution[Sexp,Sexp,Sexp,Int,ResolutionBasis.kernel.type] =
        new Resolution(
          0,
          (lemmas.map { _.thm } ++
            List(Unary(FOL.Neg(),conjecture))).flatMap { cnfSexp(_).toList },
          ResolutionBasis.ithmF,
          ResolutionBasis.interpretation,
          sexpPrinter)
      val thms = res.distance_nextThms.takeWhile(_.isDefined).flatten
      thms.map { _._2 }.filter { _.isContradiction }.headOption.map {
        _ => new ResTheorem {
          override val thm = conjecture
          override val definitions = defns
          override val resolution = res
        }
      }
    }
  }

  def define(theorem: Theorem): Option[Theorem] = {
    val fol = theorem.thm
    def strip(args: List[Term[Sexp,Sexp]])(fol: FOLSexp): Option[(Sexp, FOLSexp)] =
      fol match {
        case Bnding(FOL.Exists, f, body) =>
          val args2 = args.reverse
          (f,body.inst {
            case f2 =>
              if (f === f2)
                Fun(f,args2)
              else Var(f2)
          }).some
        case Bnding(FOL.All, v, p) => strip(Var[Sexp,Sexp](v)::args)(p)
        case _ => None
      }
    strip(List())(theorem.thm).map {
      case (f, body) =>
        new Theorem {
          val definitions = IMap.singleton(f,body)
          val thm = body
        }
    }
  }

  def unfresh(x: FOL.Fresh[Sexp]) = SexpCons(x.origin, SexpNumber(x.get))

  def cnfSexp(fol: FOLSexp) = {
    val (bnds,mat,_) = Matrix.quantPull(FOL.toNNF(fol))
    val bnds2 = bnds.map { _.rightMap(unfresh(_)) }
    val mat2 = FOL.trimap(mat)(v => v.fold(v => v, unfresh(_)),f => f,p => p)
    val mat3 = Matrix.skolemize(bnds2, mat2)
    CNF.cnf(
      FOL.trimap(mat3)(v => v, _.fold(v => v, f => f), p => p),
      SexpSymbol("="))
  }

  def sepOfString(sepInst: String) = {
    val sexp = SexpParser.parse(sepInst)
    SExpr.lambdaOfSExpr(sexp,true).map {
      lam => separationInst(lam)
        .map { _.success }
        .getOrElse((sexp, "cannot substitute freely").failureNel)
    }.reassociateLeft
  }

  def interactive(
    conjecture: String,
    sepInsts: List[String],
    lemmas: List[Theorem]) = {
    (sepInsts.traverseU { sepOfString(_) } |@|
      folOfString(conjecture).leftMap(_.success)) {
        (seps, conjecture) => prove(
          conjecture,
          lemmas ++ seps)
      }.reassociateLeft
  }
}
