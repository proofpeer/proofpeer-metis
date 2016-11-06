package proofpeer.metis

import scalaz._
import Scalaz._

trait Printer[V,F,P] {
  def printV(v: V): Cord
  def printF(f: F): Cord
  def printP(p: P): Cord
  def printTerm(tm: Term[V,F]): Cord
  def printAtom(atm: Atom[V,F,P]): Cord
  def printLiteral(lit: Literal[V,F,P]): Cord
  def printClause(cl: Clause[V,F,P]): Cord
}

trait DefaultPrinter[V,F,P] extends Printer[V,F,P] {
  def printTerm(tm: Term[V,F]) = tm match {
    case Var(v) => printV(v)
    case Fun(f,List()) => printF(f)
    case Fun(f,args) => printF(f) |+|
      Cord("(") |+| Cord.mkCord(",",args.map(printTerm(_)):_*) |+| Cord(")")
  }
  def printAtom(atm: Atom[V,F,P]) = atm match {
    case Eql(l,r) => printTerm(l) |+| "=" |+| printTerm(r)
    case Pred(p,args) => printP(p) |+|
      Cord("(") |+| Cord.mkCord(",",args.map(printTerm(_)):_*) |+| Cord(")")
  }
  def printLiteral(lit: Literal[V,F,P]) = lit match {
    case Literal(true, atm) => printAtom(atm)
    case Literal(false, atm) => Cord("~") |+| printAtom(atm)
  }
  def printClause(cl: Clause[V,F,P]) =
    Cord("{") |+| Cord.mkCord(",", cl.lits.toList.map(printLiteral(_)):_*) |+|
  Cord("}")
}

object Debug {
  def profile[A](name: String, x: => A) = {
    val start = System.nanoTime()
    val theX = x
    val end = System.nanoTime()
//    System.out.println(name + ": " + (end - start)/1000000000.0)
    theX
  }

  import scalaz._
  import Scalaz._
  def debugShowsTerm[V,F](term: Term[V,F]) = {
    import TermInstances._
    term match {
      case strTerm: Term[String,String] => strTerm.shows
    }
  }

  def debugShowsLiteral[V,F,P](lit: Literal[V,F,P]) = {
    import LiteralInstances._
    lit match {
      case strLit: Literal[String,String,String] => strLit.shows
    }
  }

  def debugShowsClause[V,F,P](cl: Clause[V,F,P]) = {
    import ClauseInstances._
    cl match {
      case strCl: Clause[String,String,String] => strCl.shows
    }
  }

  def toAlphaNum(str: String) = {
    import java.nio.charset._
    val ascii =
      new String(str.getBytes(StandardCharsets.UTF_8),StandardCharsets.US_ASCII)
    for (
      c       <- ascii;
      escaped <- if (c == '0') "00"
                 else if (c.isLetterOrDigit) c.toString
                 else "0" ++ c.toInt.shows)
    yield escaped
  }

  def TPTPOfTerm[V:Show,F:Show](tm: Term[V,F]): Cord =
    tm match {
      case Var(x) =>
        Cord("V") ++ Cord(toAlphaNum(x.shows))
      case Fun(f,List()) =>
        Cord("f") ++ Cord(toAlphaNum(f.shows))
      case Fun(f,args) =>
        val argsStrs = args.map(TPTPOfTerm(_))
        Cord("f") ++ Cord(toAlphaNum(f.shows)) ++
        Cord("(") ++ Cord.mkCord(",",argsStrs:_*) ++ Cord(")")
    }

  def TPTPOfAtom[V:Show,F:Show,P:Show](atom: Atom[V,F,P]) =
    atom match {
      case Eql(x,y) =>
        TPTPOfTerm(x) ++ "=" ++ TPTPOfTerm(y)
      case Pred(p,List()) => Cord("f") ++ Cord(toAlphaNum(p.shows))
      case Pred(p,args) =>
        val argsStrs = args.map(TPTPOfTerm(_))
        Cord("f") ++ Cord(toAlphaNum(p.shows)) ++ Cord("(") ++
        Cord.mkCord(",",argsStrs:_*) ++ Cord(")")
    }

  def TPTPOfLiteral[V:Show,F:Show,P:Show](lit: Literal[V,F,P]) =
    lit match {
      case Literal(true,atm)  => TPTPOfAtom(atm)
      case Literal(false,atm) => Cord("~ ") ++ TPTPOfAtom(atm)
    }

  def TPTPOfClause[V:Show,F:Show,P:Show](clause: Clause[V,F,P]) =
    Cord.mkCord(" | ",clause.lits.toList.map(TPTPOfLiteral(_)):_*)
}
