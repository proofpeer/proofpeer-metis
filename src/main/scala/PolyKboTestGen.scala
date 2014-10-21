package proofpeer.metis

import scala.collection.generic.{CanBuildFrom}
import scala.io.{Source}
import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.{PagedSeqReader}
import scalaz._

object KboTestCodeGen {
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
    val testIn        = KboTestCodeGen.getClass().getResourceAsStream("/kboIn")
    val testSource    = Source.fromInputStream(testIn).mkString
    val resultsIn     = KboTestCodeGen.getClass().getResourceAsStream("/kboOut")
    val resultsSource = Source.fromInputStream(resultsIn).mkString
    val testTokens    = new parser.lexical.Scanner(testSource)
    val resultsTokens = new parser.lexical.Scanner(resultsSource)
    val testCases     = parser.rep(parser.parseTermPair)(testTokens).get
    val testResults   = parser.rep(parser.parseResult)(resultsTokens).get
    class ordFun[V,F](implicit
      ordV:Order[V],
      ordF:Order[F]) extends Order[Fun[V,F]] {
      def order(fun1: Fun[V,F],fun2: Fun[V,F]) =
        (fun1.args.length,fun1.f) ?|? (fun2.args.length,fun2.f)
    }
    implicit val ordFun = new ordFun[String,String]
    val kb = new KnuthBendix[String,String]((_:Fun[String,String]) => 1:Int)
    for { (((tm1,tm2),expected),i) <-
      testCases zip
      testResults zip
      (1 to testCases.length) } {
      println(
        "    val tm" + (i*2-1) + ": Term[String,String] =" +
          "\n      " + termPP(tm1))
      println(
        "    val tm" + (i*2) + ": Term[String,String] =" +
          "\n      " + termPP(tm2))
      println("    val wtm" + (i*2-1) + " = kb.WTerm(tm" + (i*2-1) + ")")
      println("    val wtm" + (i*2) + " = kb.WTerm(tm" + (i*2) + ")")
      println(
        "    assert(kb.KBOrder.tryCompare(wtm" + (i*2-1) + ", " +
        "wtm" + (i*2) + ") == " + expected + ")\n")
    }
  }
}
