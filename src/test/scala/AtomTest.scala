package proofpeer.metis.testing.atom

import proofpeer.metis.atom._
import org.scalatest.{FlatSpec}
import scalaz._
import scalaz.std._
import Scalaz._
import Ordering._

class AtomSpec extends FlatSpec {
  "Knuth Bendix Ordering port" should " agree with original test suite" in {
    class ordComb[V,F](implicit
      ordInt:Order[Int],
      ordV:Order[V],
      ordF:Order[F]) extends Order[Comb[V,F]] {
      def order(comb1: Comb[V,F],comb2: Comb[V,F]) =
        BatOrder(ordInt).lexico(ordF)(
          (comb1.args.length,comb1.f),
          (comb2.args.length,comb2.f))
    }
    implicit val ordComb = new ordComb[String,String]
    val kb = new KnuthBendix[String,String]((_:Comb[String,String]) => 1:Int)

    val atm1: Atom[String,String] =
      Comb("f", List(
        Comb("a", List())))
    val atm2: Atom[String,String] =
      Comb("g", List(
        Comb("b", List())))
    val watm1 = kb.WAtom(atm1)
    val watm2 = kb.WAtom(atm2)
    assert(kb.KBOrder.tryCompare(watm1, watm2) == Some(LT))

    val atm3: Atom[String,String] =
      Comb("f", List(
        Comb("a", List()),
        Comb("b", List())))
    val atm4: Atom[String,String] =
      Comb("g", List(
        Comb("b", List())))
    val watm3 = kb.WAtom(atm3)
    val watm4 = kb.WAtom(atm4)
    assert(kb.KBOrder.tryCompare(watm3, watm4) == Some(GT))

    val atm5: Atom[String,String] =
      Comb("f", List(Var("x")))
    val atm6: Atom[String,String] =
      Comb("g", List(
        Comb("a", List())))
    val watm5 = kb.WAtom(atm5)
    val watm6 = kb.WAtom(atm6)
    assert(kb.KBOrder.tryCompare(watm5, watm6) == None)

    val atm7: Atom[String,String] =
      Comb("f", List(
        Comb("a", List()),
        Var("x")))
    val atm8: Atom[String,String] =
      Comb("g", List(Var("x")))
    val watm7 = kb.WAtom(atm7)
    val watm8 = kb.WAtom(atm8)
    assert(kb.KBOrder.tryCompare(watm7, watm8) == Some(GT))

    val atm9: Atom[String,String] =
      Comb("f", List(Var("x")))
    val atm10: Atom[String,String] =
      Comb("g", List(Var("x")))
    val watm9 = kb.WAtom(atm9)
    val watm10 = kb.WAtom(atm10)
    assert(kb.KBOrder.tryCompare(watm9, watm10) == Some(LT))

    val atm11: Atom[String,String] =
      Comb("f", List(Var("x")))
    val atm12: Atom[String,String] =
      Comb("f", List(Var("x")))
    val watm11 = kb.WAtom(atm11)
    val watm12 = kb.WAtom(atm12)
    assert(kb.KBOrder.tryCompare(watm11, watm12) == Some(EQ))

    val atm13: Atom[String,String] =
      Comb("+", List(Var("x"),Var("y")))
    val atm14: Atom[String,String] =
      Comb("+", List(Var("x"),Var("x")))
    val watm13 = kb.WAtom(atm13)
    val watm14 = kb.WAtom(atm14)
    assert(kb.KBOrder.tryCompare(watm13, watm14) == None)

    val atm15: Atom[String,String] =
      Comb("+", List(
        Comb("+", List(Var("x"),Var("y"))),
        Var("x")))
    val atm16: Atom[String,String] =
      Comb("+", List(
        Comb("+", List(Var("y"),Var("x"))),
        Var("x")))
    val watm15 = kb.WAtom(atm15)
    val watm16 = kb.WAtom(atm16)
    assert(kb.KBOrder.tryCompare(watm15, watm16) == None)

    val atm17: Atom[String,String] =
      Comb("+", List(
        Comb("+", List(Var("x"),Var("y"))),
        Var("x")))
    val atm18: Atom[String,String] =
      Comb("+", List(
        Comb("*", List(Var("y"),Var("x"))),
        Var("x")))
    val watm17 = kb.WAtom(atm17)
    val watm18 = kb.WAtom(atm18)
    assert(kb.KBOrder.tryCompare(watm17, watm18) == Some(GT))

    val atm19: Atom[String,String] =
      Comb("a", List())
    val atm20: Atom[String,String] =
      Var("x")
    val watm19 = kb.WAtom(atm19)
    val watm20 = kb.WAtom(atm20)
    assert(kb.KBOrder.tryCompare(watm19, watm20) == None)

    val atm21: Atom[String,String] =
      Comb("f", List(
        Comb("a", List())))
    val atm22: Atom[String,String] =
      Var("x")
    val watm21 = kb.WAtom(atm21)
    val watm22 = kb.WAtom(atm22)
    assert(kb.KBOrder.tryCompare(watm21, watm22) == None)

    val atm23: Atom[String,String] =
      Comb("f", List(
        Var("x"),
        Comb("f", List(Var("y"),Var("z")))))
    val atm24: Atom[String,String] =
      Comb("f", List(
        Comb("f", List(Var("x"),Var("y"))),
        Var("z")))
    val watm23 = kb.WAtom(atm23)
    val watm24 = kb.WAtom(atm24)
    assert(kb.KBOrder.tryCompare(watm23, watm24) == Some(LT))

    val atm25: Atom[String,String] =
      Comb("f", List(
        Comb("g", List(
            Var("x"),
            Comb("a", List())))))
    val atm26: Atom[String,String] =
      Comb("f", List(
        Comb("h", List(
            Comb("a", List()),
            Var("x")))))
    val watm25 = kb.WAtom(atm25)
    val watm26 = kb.WAtom(atm26)
    assert(kb.KBOrder.tryCompare(watm25, watm26) == Some(LT))

    val atm27: Atom[String,String] =
      Comb("f", List(
        Comb("g", List(
            Comb("a", List())))))
    val atm28: Atom[String,String] =
      Comb("f", List(
        Comb("h", List(Var("x")))))
    val watm27 = kb.WAtom(atm27)
    val watm28 = kb.WAtom(atm28)
    assert(kb.KBOrder.tryCompare(watm27, watm28) == Some(LT))

    val atm29: Atom[String,String] =
      Comb("f", List(
        Comb("h", List(
            Comb("a", List())))))
    val atm30: Atom[String,String] =
      Comb("f", List(
        Comb("g", List(Var("x")))))
    val watm29 = kb.WAtom(atm29)
    val watm30 = kb.WAtom(atm30)
    assert(kb.KBOrder.tryCompare(watm29, watm30) == None)

    val atm31: Atom[String,String] =
      Comb("f", List(Var("y")))
    val atm32: Atom[String,String] =
      Comb("f", List(
        Comb("g", List(
            Comb("a", List()),
            Comb("b", List()),
            Comb("c", List())))))
    val watm31 = kb.WAtom(atm31)
    val watm32 = kb.WAtom(atm32)
    assert(kb.KBOrder.tryCompare(watm31, watm32) == None)

    val atm33: Atom[String,String] =
      Comb("+", List(
        Comb("*", List(Var("x"),Var("y"))),
        Comb("*", List(Var("x"),Var("z")))))
    val atm34: Atom[String,String] =
      Comb("*", List(
        Var("x"),
        Comb("+", List(Var("y"),Var("z")))))
    val watm33 = kb.WAtom(atm33)
    val watm34 = kb.WAtom(atm34)
    assert(kb.KBOrder.tryCompare(watm33, watm34) == Some(GT))
  }
}
