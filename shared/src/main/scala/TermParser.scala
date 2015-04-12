package proofpeer.metis

import proofpeer.metis._
import scala.collection.immutable.StringOps
import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scalaz._
import Scalaz._

class TermParsers extends StandardTokenParsers {
  lexical.delimiters += ("(",")",",",".","=","~","|")
  lexical.reserved   += "fof"

  type StringTerm = Term[String,String]
  type StringAtom = Atom[String,String,String]
  type StringLit  = Literal[String,String,String]

  private val parseUpcaseId = {
    acceptIf {
      case lexical.Identifier(chars) => new StringOps(chars)(0).isUpper
      case _ => false
    } {
      case _ => "Upper-case identifier expected."
    }
  }

  private val parseLowcaseId = {
    acceptIf {
      case lexical.Identifier(chars) => new StringOps(chars)(0).isLower
      case _ => false
    } {
      case _ => "Lower-case identifier expected."
    }
  }

  private val parseArgs:Parser[List[StringTerm]] = {
    ("(" ~> repsep(parseTerm,",") <~ ")") |
    (success(List()))
  }

  val parseTerm:Parser[StringTerm] = {
    (for {
      id   <- parseLowcaseId
      args <- parseArgs
    }
    yield Fun(id.chars,args)) |
    parseUpcaseId.map { id => Var(id.chars) }
  }

  val parseAtom:Parser[StringAtom] = {
    (for {
      lhs <- parseMaybeBracketed(parseTerm)
      _   <- ("=":Parser[String])
      rhs <- parseMaybeBracketed(parseTerm)
    }
    yield Eql[String,String,String](lhs,rhs)) |
    (for {
      id   <- parseLowcaseId
      args <- parseArgs
    }
    yield Pred(id.chars,args))
  }

  def parseMaybeBracketed[A](parser: Parser[A]):Parser[A] = {
    ("(" ~> parseMaybeBracketed(parser) <~ ")") | parser
  }

  val parseLit:Parser[StringLit] = {
    ("~" ~> parseMaybeBracketed(parseAtom)).map { atm => Literal(false,atm) } |
    parseMaybeBracketed(parseAtom).map { atm => Literal(true,atm) }
  }

  val parseCNF:Parser[(String,String,List[StringLit])] =
    "fof" ~> "(" ~> (for {
      name <- parseLowcaseId.map(_.chars) | numericLit;
      if (name.charAt(0) != '-');
      role <- "," ~> ident <~ ",";
      lits <- repsep(parseLit,"|") <~ ")" <~ "."
    }
    yield (name,role,lits))

  val parseCNFs:Parser[List[(String,String,List[StringLit])]] = parseCNF.*
}
