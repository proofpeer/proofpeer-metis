object ThmTest {
  def main(args: Array[String]) {
    import scalaz._
    import Scalaz._
    import proofpeer.metis._
    val termX = Var[Int,Int](0)
    val termY = Var[Int,Int](1)
    val termZ = Var[Int,Int](2)
    val k = new Thm.Kernel[Int,Int,Int]
    val thm = k.axiom(Set[Literal[Int,Int,Int]](
      Literal(true,Eql(termX,termY)),
      Literal(true,Eql(termY,termZ)),
      Literal(false,Eql(termZ,termY))
    ))
    System.out.println(k.removeSym(thm).clause)
  }
}
