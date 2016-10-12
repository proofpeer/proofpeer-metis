package proofpeer.metis.fol.sttzf

import org.ensime.sexp.{ SexpParser, SexpCompactPrinter => SexpPrinter }
import org.ensime.sexp.{ Sexp, SexpCons, SexpSymbol, SexpNumber }

import proofpeer.metis. { SExpr => _, Pred => _, _ }
import proofpeer.metis.fol._

import FOL.Instances._

import scalaz._
import Scalaz._

object ZFProver {
  def folOfString(str: String):
      ValidationNel[(Sexp,String),FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]] =
    SExpr.folOfSExpr(SexpParser.parse(str),true)

  abstract sealed class Axiom {
    val thm: FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]
  }
  val extensionality = new Axiom {
    override val Success(thm) =
      folOfString("(<-> (= a b) (! x (<-> (vin x A) (vin x B))))")
  }
  val upair = new Axiom {
    override val Success(thm) =
      folOfString("(ex P (! x (<-> (vin x P) (or (= x a) (= y a)))))")
  }
  val fUnion = new Axiom {
    override val Success(thm) =
      folOfString("(ex U (! x (<-> (vin x P) (or (vin x A) (vin x B)))))")
  }
  val powerset = new Axiom {
    override val Success(thm) =
      folOfString("(ex P (! X (<-> ((vin) X P) (! x (-> (vin x X) (vin x A))))))")
  }
  val Success(sepScheme) =
    folOfString("(ex S (! x (<-> (vin x S) (and (vin x A) (P x)))))")

  def separationInst(
    inst: Term[Sexp,Sexp] => FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]) = {
    (FOL.instPred1(sepScheme) {
      case (p,x) => if (p === SexpSymbol("P")) inst(x) else Pred(p,List(x))
    }).map { ax => new Axiom { override val thm = ax }}
  }

  def unfresh(x: FOL.Fresh[Sexp]) = SexpCons(x.origin, SexpNumber(x.get))

  def cnfSexp(fol: FOL[Sexp, Sexp, Sexp, FOL.Neg, FOL.Binder]) = {
    val (bnds,mat,_) = Matrix.quantPull(FOL.toNNF(fol))
    val bnds2 = bnds.map { _.rightMap(unfresh(_)) }
    val mat2 = FOL.trimap(mat)(v => v.fold(v => v, unfresh(_)),f => f,p => p)
    val mat3 = Matrix.skolemize(bnds2, mat2)
  }

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

  def sexpPrinter = new DefaultPrinter[Sexp,Sexp,Sexp] {
    def printV(v: Sexp) = Cord(SexpPrinter(v))
    def printF(f: Sexp) = Cord(SexpPrinter(f))
    def printP(p: Sexp) = Cord(SexpPrinter(p))
  }

  case class SExprResolution(
    axioms: List[Axiom],
    lemmas: List[SExprResolution],
    conjecture: FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]) {
    implicit val ordFun = ResolutionBasis.ordFun
    implicit val kbo = ResolutionBasis.kbo
    val res: Resolution[Sexp,Sexp,Sexp,Int,ResolutionBasis.kernel.type] =
      new Resolution(
        0,
        (axioms.map {_.thm} ++ lemmas.map { lemma =>
          lemma.thm.map { _ => lemma.conjecture}.get
        } ++ List(Unary(FOL.Neg(),conjecture))).flatMap { cnfSexp(_).toList },
        ResolutionBasis.ithmF,
        ResolutionBasis.interpretation,
        sexpPrinter
      )
    val thms = res.distance_nextThms.takeWhile(_.isDefined).flatten
    lazy val thm = thms.map { _._2}.filter { _.isContradiction }.headOption
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
    sepInsts: List[String],
    axioms: List[Axiom],
    lemmas: List[SExprResolution],
    conjecture: String) = {
    (sepInsts.traverseU { sepOfString(_) } |@|
      folOfString(conjecture).leftMap(_.success)) {
        (seps, conjecture) => SExprResolution(
          (axioms ++ seps),
          lemmas,
          Unary(FOL.Neg(),conjecture))
      }
  }
}
