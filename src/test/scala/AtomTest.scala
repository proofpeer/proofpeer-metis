package proofpeer.metis.testing

import proofpeer.metis._
import org.scalatest.{FlatSpec}
import scalaz._
import scalaz.std._
import Scalaz._
import Ordering._

class AtomSpec extends FlatSpec {
  "Knuth Bendix Ordering port" should " agree with original test suite" in
  {
    implicit val ordFun = KnuthBendix.precedenceOrder[String,String]
    val kb = new KnuthBendix[String,String]({ case (_,_) => 1:Int })

    val tm1: Term[String,String] =
      Fun("f", List(
        Fun("a", List())))
    val tm2: Term[String,String] =
      Fun("g", List(
        Fun("b", List())))
    assert(kb.tryCompare(tm1, tm2) == Some(LT))

    val tm3: Term[String,String] =
      Fun("f", List(
        Fun("a", List()),
        Fun("b", List())))
    val tm4: Term[String,String] =
      Fun("g", List(
        Fun("b", List())))
    assert(kb.tryCompare(tm3, tm4) == Some(GT))

    val tm5: Term[String,String] =
      Fun("f", List(Var("x")))
    val tm6: Term[String,String] =
      Fun("g", List(
        Fun("a", List())))
    assert(kb.tryCompare(tm5, tm6) == None)

    val tm7: Term[String,String] =
      Fun("f", List(
        Fun("a", List()),
        Var("x")))
    val tm8: Term[String,String] =
      Fun("g", List(Var("x")))
    assert(kb.tryCompare(tm7, tm8) == Some(GT))

    val tm9: Term[String,String] =
      Fun("f", List(Var("x")))
    val tm10: Term[String,String] =
      Fun("g", List(Var("x")))
    assert(kb.tryCompare(tm9, tm10) == Some(LT))

    val tm11: Term[String,String] =
      Fun("f", List(Var("x")))
    val tm12: Term[String,String] =
      Fun("f", List(Var("x")))
    assert(kb.tryCompare(tm11, tm12) == Some(EQ))

    val tm13: Term[String,String] =
      Fun("+", List(Var("x"),Var("y")))
    val tm14: Term[String,String] =
      Fun("+", List(Var("x"),Var("x")))
    assert(kb.tryCompare(tm13, tm14) == None)

    val tm15: Term[String,String] =
      Fun("+", List(
        Fun("+", List(Var("x"),Var("y"))),
        Var("x")))
    val tm16: Term[String,String] =
      Fun("+", List(
        Fun("+", List(Var("y"),Var("x"))),
        Var("x")))
    assert(kb.tryCompare(tm15, tm16) == None)

    val tm17: Term[String,String] =
      Fun("+", List(
        Fun("+", List(Var("x"),Var("y"))),
        Var("x")))
    val tm18: Term[String,String] =
      Fun("+", List(
        Fun("*", List(Var("y"),Var("x"))),
        Var("x")))
    assert(kb.tryCompare(tm17, tm18) == Some(GT))

    val tm19: Term[String,String] =
      Fun("a", List())
    val tm20: Term[String,String] =
      Var("x")
    assert(kb.tryCompare(tm19, tm20) == None)

    val tm21: Term[String,String] =
      Fun("f", List(
        Fun("a", List())))
    val tm22: Term[String,String] =
      Var("x")
    assert(kb.tryCompare(tm21, tm22) == None)

    val tm23: Term[String,String] =
      Fun("f", List(
        Var("x"),
        Fun("f", List(Var("y"),Var("z")))))
    val tm24: Term[String,String] =
      Fun("f", List(
        Fun("f", List(Var("x"),Var("y"))),
        Var("z")))
    assert(kb.tryCompare(tm23, tm24) == Some(LT))

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
    assert(kb.tryCompare(tm25, tm26) == Some(LT))

    val tm27: Term[String,String] =
      Fun("f", List(
        Fun("g", List(
            Fun("a", List())))))
    val tm28: Term[String,String] =
      Fun("f", List(
        Fun("h", List(Var("x")))))
    assert(kb.tryCompare(tm27, tm28) == Some(LT))

    val tm29: Term[String,String] =
      Fun("f", List(
        Fun("h", List(
            Fun("a", List())))))
    val tm30: Term[String,String] =
      Fun("f", List(
        Fun("g", List(Var("x")))))
    assert(kb.tryCompare(tm29, tm30) == None)

    val tm31: Term[String,String] =
      Fun("f", List(Var("y")))
    val tm32: Term[String,String] =
      Fun("f", List(
        Fun("g", List(
            Fun("a", List()),
            Fun("b", List()),
            Fun("c", List())))))
    assert(kb.tryCompare(tm31, tm32) == None)

    val tm33: Term[String,String] =
      Fun("+", List(
        Fun("*", List(Var("x"),Var("y"))),
        Fun("*", List(Var("x"),Var("z")))))
    val tm34: Term[String,String] =
      Fun("*", List(
        Var("x"),
        Fun("+", List(Var("y"),Var("z")))))
    assert(kb.tryCompare(tm33, tm34) == Some(GT))
  }
}
