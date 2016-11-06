package proofpeer.metis.fol

import proofpeer.metis. { Pred => MPred, _ }
import proofpeer.metis.util.Identified

import scalaz._
import Scalaz._

import proofpeer.metis.fol.FOL.Fresh

trait TPTPPrinter[V,F,P] extends DefaultPrinter[V,F,P] {
  implicit def orderV: Order[V]
  implicit def orderF: Order[F]
  implicit def orderP: Order[P]

  def tptpOfV(v: V): Option[Cord]
  def tptpOfF(f: F): Option[Cord]
  def tptpOfP(p: P): Option[Cord]

  def isLetter(c:Char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  def isDigit(c:Char) = c >= '0' && c <= '9'

  def isVarId(sym:Cord) = {
    val str = sym.toString
    (str.length > 0 && str.charAt(0).isUpper
      && str.forall(c => isLetter(c) || isDigit(c)))
  }
  def isFnId(sym:Cord) = {
    val str = sym.toString
    (str.length > 0 && str.charAt(0).isLower
      && str.forall(c => isLetter(c) || isDigit(c)))
  }

  val vmap = new Identified[V]
  val fmap = new Identified[F]
  val pmap = new Identified[P]

  def printV(v: V): Cord =
    (tptpOfV(v) >>= { v =>
      if (isVarId(v))
        Some(v)
      else if (isFnId(v))
        Some(Cord("V_") |+| v)
      else None
    }).getOrElse { Cord("I_") |+| vmap.getIndex(v).show }

  def printF(f: F): Cord =
    (tptpOfF(f) >>= { f =>
      if (isFnId(f))
        Some(f)
      else if (isVarId(f))
        Some(Cord("f_") |+| f)
      else None
    }).getOrElse { Cord("i_") |+| fmap.getIndex(f).show }

  def printP(p: P): Cord =
    tptpOfP(p).filter(isFnId(_)).getOrElse { Cord("i_") |+| pmap.getIndex(p).show }

  def tptpOfTerm(tm: Term[V,F]): Cord = tm match {
    case Var(x) => printV(x)
    case Fun(f,List()) => printF(f)
    case Fun(f,args) =>
      val argStrs = args.map(tptpOfTerm(_))
      Cord(printF(f)) |+| Cord("(") |+| Cord.mkCord(",",argStrs:_*) |+| Cord(")")
  }
  def tptpOfAtom(atm: Atom[V,F,P]): Cord = atm match {
    case Eql(x,y) =>
      tptpOfTerm(x) |+| "=" |+| tptpOfTerm(y)
    case MPred(p,List()) => printP(p)
    case MPred(p,args) =>
      val argStrs = args.map(tptpOfTerm(_))
      Cord(printP(p)) |+| Cord("(") |+| Cord.mkCord(",",argStrs:_*) |+| Cord(")")
  }
  def tptpOfLiteral(lit: Literal[V,F,P]): Cord = lit match {
    case Literal(true,atm) => tptpOfAtom(atm)
    case Literal(false,atm) => Cord("~") |+| tptpOfAtom(atm)
  }
  def tptpOfClause(cl: Clause[V,F,P]) =
    Cord.mkCord(" | ", cl.lits.toList.map(tptpOfLiteral(_)):_*)
}
