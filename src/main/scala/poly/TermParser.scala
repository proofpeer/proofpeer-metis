package proofpeer.metis.poly.term

import proofpeer.metis.atom._
import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scalaz._
import scalaz.std._

class TermParser extends StandardTokenParsers {
  lexical.reserved   += ("Fn","Var","SOME","NONE","LESS","EQUAL","GREATER")
  lexical.delimiters += ("(",")",",","[","]")

  type PolyAtom = Atom[String,String]

  val parseAtom:Parser[PolyAtom] =
    ("Var" ~> stringLit map { Var[String,String](_) }) |
    ("Fn" ~>
      "(" ~>
      (for {
        i    <- stringLit;
        _    <- "," ~ "[";
        args <- repsep(parseAtom,",") <~ "]"
      }
      yield (Comb(i,args)))
      <~ ")") |
    ("(" ~> parseAtom <~ ")")

  val parseAtomPair:Parser[Tuple2[PolyAtom,PolyAtom]] = {
    for {
      _   <- "(":Parser[Any]
      fst <- parseAtom
      _   <- ",":Parser[Any]
      snd <- parseAtom
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
