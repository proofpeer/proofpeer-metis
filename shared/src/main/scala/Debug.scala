package proofpeer.metis

object Debug {
  def profile[A](name: String, x: => A) = {
    val start = System.nanoTime()
    val theX = x
    val end = System.nanoTime()
//    System.out.println(name + ": " + (end - start)/1000000000.0)
    theX
  }
}
