package proofpeer.metis

object Debug {
  def profile[A](name: String, x: => A) = {
    val start = System.nanoTime()
    val theX = x
    val end = System.nanoTime()
//    System.out.println(name + ": " + (end - start)/1000000000.0)
    theX
  }

  import scalaz._
  import Scalaz._
  import ClauseInstances._
  def debugShowsClause[V,F,P](cl: Clause[V,F,P]) = {
    cl match {
      case strCl: Clause[String,String,String] => strCl.shows
    }
  }
}
