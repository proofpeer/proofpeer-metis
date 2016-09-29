package proofpeer.metis.fol.sttzf

import org.ensime.sexp.{ SexpParser, SexpCompactPrinter => SexpPrinter }
import org.ensime.sexp.{ Sexp, SexpCons, SexpNumber }

import proofpeer.metis. { SExpr => _, _ }
import proofpeer.metis.fol._

import FOL.Instances._

import scalaz._
import Scalaz._

object ZFProver {
  def folOfString(str: String):
      ValidationNel[(Sexp,String),FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]] =
    SExpr.folOfSExpr(SexpParser.parse(str),true)

  val extensionality =
    folOfString("(<-> (= a b) (! x (<-> (vin x A) (vin x B))))")
  val upair =
    folOfString("(ex P (! x (<-> (vin x P) (or (= x a) (= y a)))))")
  val funion =
    folOfString("(ex U (! x (<-> (vin x P) (or (vin x A) (vin x B)))))")
  val powerset =
    folOfString("(ex P (! X (<-> ((vin) X P) (! x (-> ((vin) x X) ((vin) x A))))))")
  def separation(
    inst: (Sexp, List[Term[Sexp,Sexp]]) => FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]) =
    folOfString("(ex S (! x (<-> (vin x S) (and (vin x A) (P x)))))").map {
      fol => FOL.instPred(fol)(inst)
    }

  def unfresh(x: FOL.Fresh[Sexp]) = SexpCons(x.origin, SexpNumber(x.get))

  def cnfSexp(fol: FOL[Sexp, Sexp, Sexp, FOL.Neg, FOL.Binder]) = {
    val (bnds,mat,_) = Matrix.quantPull(FOL.toNNF(fol))
    val bnds2 = bnds.map { _.rightMap(unfresh(_)) }
    val mat2 = FOL.trimap(mat)(v => v.fold(v => v, unfresh(_)),f => f,p => p)
    val mat3 = Matrix.skolemize(bnds2, mat2)
    CNF.cnf(FOL.trimap(mat3)(v => v, _.fold(v => v, f => f), p => p))
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
    axioms: List[FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]],
    lemmas: List[SExprResolution],
    conjecture: FOL[Sexp,Sexp,Sexp,FOL.Neg,FOL.Binder]) {
    implicit val ordFun = ResolutionBasis.ordFun
    implicit val kbo = ResolutionBasis.kbo
    val res: Resolution[Sexp,Sexp,Sexp,Int,ResolutionBasis.kernel.type] =
      new Resolution(
        0,
        (axioms ++ lemmas.map { lemma =>
          lemma.thm.map { _ => lemma.conjecture}.get
        } ++ List(Unary(FOL.Neg(),conjecture))).flatMap { cnfSexp(_).toList },
        ResolutionBasis.ithmF,
        ResolutionBasis.interpretation,
        sexpPrinter
      )
    val thms = res.distance_nextThms.takeWhile(_.isDefined).flatten
    lazy val thm = thms.map { _._2}.filter { _.isContradiction }.headOption
  }
}
