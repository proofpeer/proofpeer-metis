package proofpeer.metis

object SimpleParse {
  val parser = new proofpeer.metis.TermParsers()

  def parse(str:String) = {
    val tokens = new parser.lexical.Scanner(str)
    parser.parseLit(tokens)
  }
}
