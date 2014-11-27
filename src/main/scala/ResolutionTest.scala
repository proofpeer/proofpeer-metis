package proofpeer.metis

import scala.language.implicitConversions
import scalaz._
import Scalaz._

object ResolutionTest {
  val parser = new proofpeer.metis.TermParsers()

  def parse(str:String) = {
    val tokens = new parser.lexical.Scanner(str)
    parser.parseLit(tokens)
  }

  val cl1 = List("P(x0, F1(x0))").map(parse)
  val cl2 = List("~P(J, x2)").map(parse)

  implicit val ordFun = new Order[Fun[String,String]] {
    def order(f: Fun[String,String], g: Fun[String,String]) = {
      (f.args.length,f.f) ?|? (g.args.length,g.f)
    }
  }

  val kbo    = new KnuthBendix[String,String]((_,_) => 1)
  val litOrd = new LiteralOrdering[String,String,String](kbo,x => x)
  val kernel = new Kernel[String,String,String]
  val factor = new Factor[String,String,String]
  val ithmF  = new IThmFactory[String,String,String,Int,kernel.type](
    kernel,
    0,
    n => (n+1, "gen" + n),
    litOrd,
    factor)
  val activeF = new ActiveFactory[String,String,String,Int,kernel.type,ithmF.type](
    ithmF,litOrd)
  val active = new activeF.Active
  val waitingF = new WaitingFactory[String,String,String,Int,kernel.type,ithmF.type](
    ithmF, litOrd)
  val waiting = new waitingF.Waiting

  val thm1 = ithmF.axiom(Clause(Set() ++ cl1.map(_.get)))
  val thm2 = ithmF.axiom(Clause(Set() ++ cl2.map(_.get)))
}
