package proofpeer.metis

import scala.collection.generic.{CanBuildFrom}
import scala.io.{Source}
import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.{PagedSeqReader}
import scalaz._

object PolyNetTestCodeGen {
  def intersperse[A](xs:List[A],sep:A):List[A] = {
    val (head,tail) = xs.splitAt(1)
    head ++ (tail flatMap (x => List(sep,x)))
  }

  def termPP(tm: Term[String,String]) = {
    def pp(tm: Term[String,String], i: Int): String = {
      tm match {
        case Var(s)   => "Var(\"" + s + "\")"
        case Fun(f,args) if args forall {
          case Var(_) => true
          case _      => false
        } =>
          "Fun(" + "\"" + f + "\"" +
          ", List(" +
          intersperse(
            args map { arg => pp(arg,i+2) },
            ",").mkString +
          "))"
        case Fun(f,args) =>
          "Fun(" + "\"" + f + "\"" +
          ", List(\n" +
          " " * (i+2) +
          intersperse(
            args map { arg => pp(arg,i+4) },
            ",\n" + " " * (i+2)).mkString +
          "))"
      }
    }
    pp(tm,6)
  }

  def main(args: Array[String]) {
    import Scalaz._
    val parser        = new proofpeer.metis.poly.TermParser()
    val testIn        = PolyNetTestCodeGen.getClass()
      .getResourceAsStream("/TermExamples")
    val testSource    = Source.fromInputStream(testIn).mkString
    val testTokens    = new parser.lexical.Scanner(testSource)
    val testCases     = parser.rep(parser.parseTerm)(testTokens).get
    for { (tm,i) <- testCases zip (1 to testCases.length) } {
      println(
        "    val tm" + i + ": Term[String,String] =" +
          "\n      " + termPP(tm))
    }
  }
}
