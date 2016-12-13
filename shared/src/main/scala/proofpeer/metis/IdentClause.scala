package proofpeer.metis

import TermInstances._
import util.Identified

import scalaz._
import Scalaz._

class IdentClause[V:Order,F:Order,P:Order] {
  type Id = Int

  val vmap = new Identified[V]
  val fmap = new Identified[F]
  val pmap = new Identified[P]

  def toIdentV(v: V): Id = vmap.getIndex(v)
  def toIdentF(f: F): Id = fmap.getIndex(f)
  def toIdentP(p: P): Id = pmap.getIndex(p)

  def toIdentTerm(tm: Term[V,F]): Term[Id,Id] =
    tm.bimap(toIdentV(_), toIdentF(_))

  def toIdentAtom(atm: Atom[V,F,P]): Atom[Id,Id,Id] =
    Atom.trimap(atm)(toIdentV(_), toIdentF(_), toIdentP(_))

  def toIdentLiteral(lit: Literal[V,F,P]): Literal[Id,Id,Id] =
    Literal.trimap(lit)(toIdentV(_), toIdentF(_), toIdentP(_))

  def toIdentClause(cl: Clause[V,F,P]): Clause[Id,Id,Id] = {
    Clause.trimap(cl)(toIdentV(_), toIdentF(_), toIdentP(_))
  }

  def fromIdentTerm(tm: Term[Id,Id]): Term[V,F] =
    tm.bimap(vmap.getById(_), fmap.getById(_))

  def fromIdentAtom(atm: Atom[Id,Id,Id]): Atom[V,F,P] =
    Atom.trimap(atm)(vmap.getById(_), fmap.getById(_), pmap.getById(_))

  def fromIdentLiteral(lit: Literal[Id,Id,Id]): Literal[V,F,P] =
    Literal.trimap(lit)(vmap.getById(_), fmap.getById(_), pmap.getById(_))

  def fromIdentClause(cl: Clause[Id,Id,Id]): Clause[V,F,P] = {
    Clause.trimap(cl)(vmap.getById(_), fmap.getById(_), pmap.getById(_))
  }

  def getVarById(id: Id): V = vmap.getById(id)
  def getFunById(id: Id): F = fmap.getById(id)
  def getPredById(id: Id): P = pmap.getById(id)

  def identClausePrinter(printer: Printer[V,F,P]) = new Printer[Id,Id,Id] {
    override def printV(v: Id) = printer.printV(getVarById(v))
    override def printF(f: Id) = printer.printF(getFunById(f))
    override def printP(p: Id) = printer.printP(getPredById(p))
    override def printTerm(tm: Term[Id,Id]) = printer.printTerm(fromIdentTerm(tm))
    override def printAtom(atm: Atom[Id,Id,Id]) =
      printer.printAtom(fromIdentAtom(atm))
    override def printLiteral(lit: Literal[Id,Id,Id]) =
      printer.printLiteral(fromIdentLiteral(lit))
    override def printClause(cl: Clause[Id,Id,Id]) =
      printer.printClause(fromIdentClause(cl))
  }

  def identResClausePrinter(printer: Printer[(V,Int),F,P]) =
    new Printer[(Id,Int),Id,Id] {
      override def printV(v: (Id,Int)) = {
        val (v_,idx) = v
        printer.printV((getVarById(v_),idx))
      }
      override def printF(f: Id) = printer.printF(getFunById(f))
      override def printP(p: Id) = printer.printP(getPredById(p))
      override def printTerm(tm: Term[(Id,Int),Id]) = {
        val tm_ = tm.bimap(_.leftMap(vmap.getById(_)), fmap.getById(_))
        printer.printTerm(tm_)
      }
      override def printAtom(atm: Atom[(Id,Int),Id,Id]) = {
        val atm_ = Atom.trimap(atm)(
          _.leftMap(vmap.getById(_)),
          fmap.getById(_),
          pmap.getById(_))
        printer.printAtom(atm_)
      }
      override def printLiteral(lit: Literal[(Id,Int),Id,Id]) = {
        val lit_ = Literal.trimap(lit)(
          _.leftMap(vmap.getById(_)),
          fmap.getById(_),
          pmap.getById(_))
        printer.printLiteral(lit_)
      }
      override def printClause(cl: Clause[(Id,Int),Id,Id]) = {
        val cl_ = Clause.trimap(cl)(
          _.leftMap(vmap.getById(_)),
          fmap.getById(_),
          pmap.getById(_))
        printer.printClause(cl_)
      }
  }

  implicit val ordFun = KnuthBendix.precedenceOrder[(Id,Int),Id]
  implicit val kbo = KnuthBendix.kbo[(Id,Int),Id]
  object ResolutionBasis {
    val kernel = new Kernel[(Id,Int), Id, Id]
    val factor = new Factor[(Id,Int), Id, Id]
    val litOrd = new MetisLiteralOrdering(kbo)
    val fin    = FinOrd(8)
    val vals   = Valuations[(Id,Int)](fin)
    val interpretation = Interpretation[(Id,Int),Id,Id](1000,vals)
    val ithmF  = new IThmFactory[(Id,Int),Id,Id,Id,kernel.type](
      kernel,
      0,
      { case (n,(id,idx)) => (n+1,(id,n+1)) },
      litOrd,
      factor)
  }
  def resolution(problem: List[Clause[V,F,P]], printer: Printer[(V,Id),F,P]):
      Resolution[(Id,Int),Id,Id,Id,ResolutionBasis.kernel.type] =
    new Resolution(
      0,
      problem.map { cl => Clause.trimap(toIdentClause(cl))(
        i => (i,0),
        i => i,
        i => i)
      },
      ResolutionBasis.ithmF,
      ResolutionBasis.interpretation,
      identResClausePrinter(printer))
}
