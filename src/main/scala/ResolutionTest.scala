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

  def resolution1(res:Tuple2[waitingF.Waiting,activeF.Active]) = {
    val (waiting,active) = res
    waiting.remove match {
      case Some((_,(_,ithm))) if ithm.clause.isContradiction =>
        System.out.println("DONE AND DONE!"); None
      case Some((waiting2,(dist,ithm))) =>
        val (active2, newThms) = active.add(ithm)
        val waiting3 = waiting2.add(dist,newThms)
        (waiting3,active2).some
      case None => None
    }
  }

  def iterate[A,B](init: A, f:A => Option[A]): Stream[A] = {
    init #:: (f(init) match {
      case Some(x) => iterate(x,f)
      case None    => Stream()
    })
  }
  val init = testing.tptp.Problems.SET.SET001_MINUS1.clauses.map {
    cls => ithmF.axiom(Clause(cls.toSet))
  }

  val resolves = iterate((waiting.add(0,init),active),resolution1)

  //resolves(2)._1.remove.get._2._2
  def showStep(n:Int) = {
    val (dist,thm) = ResolutionTest.resolves(n)._1.remove.get._2
    System.out.println("Removing: ")
    thm.clause.lits.foreach {
      lit => System.out.println(TermPrinter.printLiteral(lit))
    }
    (dist,thm)
  }

  def main(args: Array[String]) {
//    System.out.println(resolves.length)
    0 to 23 foreach { n => System.out.println(n); showStep(n) }
    showStep(24)
    System.out.println("END")
  }
}

// import proofpeer.metis._
// import scalaz._
// import Scalaz._
// val subsumer = new Subsumer[String,String,String,Int]
// def parse(str:String) = SimpleParse.parse(str).get
// val cl = Clause(List("~Subset(x0, x1)","~Subset(x1, x0)","Equal_sets(x1, x0)").map(parse(_)).toSet)
