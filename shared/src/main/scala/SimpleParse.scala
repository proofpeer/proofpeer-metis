package proofpeer.metis

object SimpleParse {
  val parser = new proofpeer.metis.TermParsers()

  def parse(str:String) = {
    val tokens = new parser.lexical.Scanner(str)
    parser.parseLit(tokens)
  }
  def parseClause(str:String) = {
    val tokens = new parser.lexical.Scanner(str)
    parser.parseCNF(tokens)
  }
  def parseClauses(str:String) = {
    val tokens = new parser.lexical.Scanner(str)
    parser.parseCNFs(tokens)
  }
}
