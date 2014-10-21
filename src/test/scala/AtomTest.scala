package proofpeer.metis.testing

import proofpeer.metis._
import org.scalatest.{FlatSpec}
import scalaz._
import scalaz.std._
import Scalaz._
import Ordering._

class AtomSpec extends FlatSpec {
  "Knuth Bendix Ordering port" should " agree with original test suite" in {
    class ordFun[V,F](implicit
      ordInt:Order[Int],
      ordV:Order[V],
      ordF:Order[F]) extends Order[Fun[V,F]] {
      def order(fun1: Fun[V,F],fun2: Fun[V,F]) =
        (fun1.args.length,fun1.f) ?|? (fun2.args.length,fun2.f)
    }
    implicit val ordFun = new ordFun[String,String]
    val kb = new KnuthBendix[String,String]((_:Fun[String,String]) => 1:Int)

    val tm1: Term[String,String] =
      Fun("f", List(
        Fun("a", List())))
    val tm2: Term[String,String] =
      Fun("g", List(
        Fun("b", List())))
    val wtm1 = kb.WTerm(tm1)
    val wtm2 = kb.WTerm(tm2)
    assert(kb.tryCompare(wtm1, wtm2) == Some(LT))

    val tm3: Term[String,String] =
      Fun("f", List(
        Fun("a", List()),
        Fun("b", List())))
    val tm4: Term[String,String] =
      Fun("g", List(
        Fun("b", List())))
    val wtm3 = kb.WTerm(tm3)
    val wtm4 = kb.WTerm(tm4)
    assert(kb.tryCompare(wtm3, wtm4) == Some(GT))

    val tm5: Term[String,String] =
      Fun("f", List(Var("x")))
    val tm6: Term[String,String] =
      Fun("g", List(
        Fun("a", List())))
    val wtm5 = kb.WTerm(tm5)
    val wtm6 = kb.WTerm(tm6)
    assert(kb.tryCompare(wtm5, wtm6) == None)

    val tm7: Term[String,String] =
      Fun("f", List(
        Fun("a", List()),
        Var("x")))
    val tm8: Term[String,String] =
      Fun("g", List(Var("x")))
    val wtm7 = kb.WTerm(tm7)
    val wtm8 = kb.WTerm(tm8)
    assert(kb.tryCompare(wtm7, wtm8) == Some(GT))

    val tm9: Term[String,String] =
      Fun("f", List(Var("x")))
    val tm10: Term[String,String] =
      Fun("g", List(Var("x")))
    val wtm9 = kb.WTerm(tm9)
    val wtm10 = kb.WTerm(tm10)
    assert(kb.tryCompare(wtm9, wtm10) == Some(LT))

    val tm11: Term[String,String] =
      Fun("f", List(Var("x")))
    val tm12: Term[String,String] =
      Fun("f", List(Var("x")))
    val wtm11 = kb.WTerm(tm11)
    val wtm12 = kb.WTerm(tm12)
    assert(kb.tryCompare(wtm11, wtm12) == Some(EQ))

    val tm13: Term[String,String] =
      Fun("+", List(Var("x"),Var("y")))
    val tm14: Term[String,String] =
      Fun("+", List(Var("x"),Var("x")))
    val wtm13 = kb.WTerm(tm13)
    val wtm14 = kb.WTerm(tm14)
    assert(kb.tryCompare(wtm13, wtm14) == None)

    val tm15: Term[String,String] =
      Fun("+", List(
        Fun("+", List(Var("x"),Var("y"))),
        Var("x")))
    val tm16: Term[String,String] =
      Fun("+", List(
        Fun("+", List(Var("y"),Var("x"))),
        Var("x")))
    val wtm15 = kb.WTerm(tm15)
    val wtm16 = kb.WTerm(tm16)
    assert(kb.tryCompare(wtm15, wtm16) == None)

    val tm17: Term[String,String] =
      Fun("+", List(
        Fun("+", List(Var("x"),Var("y"))),
        Var("x")))
    val tm18: Term[String,String] =
      Fun("+", List(
        Fun("*", List(Var("y"),Var("x"))),
        Var("x")))
    val wtm17 = kb.WTerm(tm17)
    val wtm18 = kb.WTerm(tm18)
    assert(kb.tryCompare(wtm17, wtm18) == Some(GT))

    val tm19: Term[String,String] =
      Fun("a", List())
    val tm20: Term[String,String] =
      Var("x")
    val wtm19 = kb.WTerm(tm19)
    val wtm20 = kb.WTerm(tm20)
    assert(kb.tryCompare(wtm19, wtm20) == None)

    val tm21: Term[String,String] =
      Fun("f", List(
        Fun("a", List())))
    val tm22: Term[String,String] =
      Var("x")
    val wtm21 = kb.WTerm(tm21)
    val wtm22 = kb.WTerm(tm22)
    assert(kb.tryCompare(wtm21, wtm22) == None)

    val tm23: Term[String,String] =
      Fun("f", List(
        Var("x"),
        Fun("f", List(Var("y"),Var("z")))))
    val tm24: Term[String,String] =
      Fun("f", List(
        Fun("f", List(Var("x"),Var("y"))),
        Var("z")))
    val wtm23 = kb.WTerm(tm23)
    val wtm24 = kb.WTerm(tm24)
    assert(kb.tryCompare(wtm23, wtm24) == Some(LT))

    val tm25: Term[String,String] =
      Fun("f", List(
        Fun("g", List(
            Var("x"),
            Fun("a", List())))))
    val tm26: Term[String,String] =
      Fun("f", List(
        Fun("h", List(
            Fun("a", List()),
            Var("x")))))
    val wtm25 = kb.WTerm(tm25)
    val wtm26 = kb.WTerm(tm26)
    assert(kb.tryCompare(wtm25, wtm26) == Some(LT))

    val tm27: Term[String,String] =
      Fun("f", List(
        Fun("g", List(
            Fun("a", List())))))
    val tm28: Term[String,String] =
      Fun("f", List(
        Fun("h", List(Var("x")))))
    val wtm27 = kb.WTerm(tm27)
    val wtm28 = kb.WTerm(tm28)
    assert(kb.tryCompare(wtm27, wtm28) == Some(LT))

    val tm29: Term[String,String] =
      Fun("f", List(
        Fun("h", List(
            Fun("a", List())))))
    val tm30: Term[String,String] =
      Fun("f", List(
        Fun("g", List(Var("x")))))
    val wtm29 = kb.WTerm(tm29)
    val wtm30 = kb.WTerm(tm30)
    assert(kb.tryCompare(wtm29, wtm30) == None)

    val tm31: Term[String,String] =
      Fun("f", List(Var("y")))
    val tm32: Term[String,String] =
      Fun("f", List(
        Fun("g", List(
            Fun("a", List()),
            Fun("b", List()),
            Fun("c", List())))))
    val wtm31 = kb.WTerm(tm31)
    val wtm32 = kb.WTerm(tm32)
    assert(kb.tryCompare(wtm31, wtm32) == None)

    val tm33: Term[String,String] =
      Fun("+", List(
        Fun("*", List(Var("x"),Var("y"))),
        Fun("*", List(Var("x"),Var("z")))))
    val tm34: Term[String,String] =
      Fun("*", List(
        Var("x"),
        Fun("+", List(Var("y"),Var("z")))))
    val wtm33 = kb.WTerm(tm33)
    val wtm34 = kb.WTerm(tm34)
    assert(kb.tryCompare(wtm33, wtm34) == Some(GT))
  }
}
