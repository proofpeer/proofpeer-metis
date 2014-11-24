package proofpeer.metis.poly

import proofpeer.metis._
import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scalaz._
import Scalaz._

class TermParser extends StandardTokenParsers {
  lexical.reserved   += ("Fn","Var","SOME","NONE","LESS","EQUAL","GREATER")

  lexical.delimiters += ("(",")",",","[","]")

  type PolyTerm = Term[String,String]

  val parseTerm:Parser[PolyTerm] =
    ("Var" ~> stringLit map { Var[String,String](_) }) |
    ("Fn" ~>
      "(" ~>
      (for {
        i    <- stringLit;
        _    <- "," ~ "[";
        args <- repsep(parseTerm,",") <~ "]"
      }
      yield (Fun(i,args)))
      <~ ")") |
    ("(" ~> parseTerm <~ ")")

  val parseTermPair:Parser[Tuple2[PolyTerm,PolyTerm]] = {
    for {
      _   <- "(":Parser[Any]
      fst <- parseTerm
      _   <- ",":Parser[Any]
      snd <- parseTerm
      _   <- ")":Parser[Any]
    }
    yield (fst,snd)
  }

  val parseOrder:Parser[Ordering] = {
    ("LESS"    ~> success(Ordering.LT)) |
    ("EQUAL"   ~> success(Ordering.EQ)) |
    ("GREATER" ~> success(Ordering.GT))
  }

  val parseResult:Parser[Option[Ordering]] = {
    ("SOME" ~> parseOrder map (Some(_))) |
    ("NONE" ~> success(None))
  }
}
