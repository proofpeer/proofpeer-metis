package proofpeer.metis.testing

import proofpeer.metis._
import org.scalatest.{FlatSpec}
import scalaz._
import Scalaz._

class NetSpec extends FlatSpec {
    val tm1: Term[String,String] =
      Fun("F", List(Var("a"),Var("b")))
    val tm2: Term[String,String] =
      Fun("F", List(
        Fun("G", List(Var("a"),Var("b"))),
        Fun("H", List(Var("c"),Var("d")))))
    val tm3: Term[String,String] =
      Fun("G", List(
        Fun("F", List(Var("x"),Var("y")))))
    val tm4: Term[String,String] =
      Fun("F", List(
        Fun("K", List(Var("x"))),
        Fun("K", List(
            Fun("L", List(Var("y")))))))
    val tm5: Term[String,String] =
      Fun("F", List(Var("a")))
    val tm6: Term[String,String] =
      Fun("F", List(Var("z"),Var("z")))
    val tm7: Term[String,String] =
      Fun("F", List(Var("z"),Var("w")))

  val net = new Nets.TermNet()
    .insert(tm1,1)
    .insert(tm2,2)
    .insert(tm3,3)
    .insert(tm4,4)
    .insert(tm5,5)

  assert(net.matches(tm1) == List(1))
  assert(net.matches(tm2) == List(1,2))
  assert(net.matches(tm3) == List(3))
  assert(net.matches(tm4) == List(1,4))
  assert(net.matched(tm6) == List(1))
  assert(net.matched(tm7) == List(1,2,4))
  assert(net.unifies(tm1) == List(1,2,4))
  assert(net.unifies(tm6) == List(1,4))
  assert(net.unifies(tm7) == List(1,2,4))
  assert(net.size == 5)
  assert(net.filter(_ => false).size == 0)
}
