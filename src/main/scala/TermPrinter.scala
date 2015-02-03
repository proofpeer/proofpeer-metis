package proofpeer.metis

import proofpeer.metis._
import scalaz._
import Scalaz._

object TermPrinter {
  def printTerm(tm: Term[String,String]): String = {
    tm match {
      case Var(v) => v
      case Fun("Multiply",List(x,y)) =>
        "(" ++ printTerm(x) ++ " * " + printTerm(y) ++ ")"
      case Fun(f,args) => f ++
        "(" ++
        intersperse(args.map(printTerm(_)),",").flatten ++
        ")"
    }
  }

  def printAtom(atom: Atom[String,String,String]): String = {
    atom match {
      case Eql(x,y)     => printTerm(x) ++ "=" ++ printTerm(y)
      case Pred(p,args) =>
        p ++
        "(" ++
        intersperse(args.map(printTerm(_)),",").flatten ++
        ")"
    }
  }

  def printLiteral(lit: Literal[String,String,String]): String = {
    (if (!lit.isPositive) "~" else "") ++ printAtom(lit.atom)
  }
}
