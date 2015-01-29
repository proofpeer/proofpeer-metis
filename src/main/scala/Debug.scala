package proofpeer.metis

object Debug {
  def stringClause[V,F,P](lits:Set[Literal[V,F,P]]) = {
    lits.map { case lit:Literal[String,String,String] =>
      TermPrinter.printLiteral(lit)
    }
  }
  def printClause[V,F,P](lits:Set[Literal[V,F,P]]) = {
    lits.foreach { case lit:Literal[String,String,String] =>
      System.out.println(TermPrinter.printLiteral(lit))
    }
    System.out.println("")
  }
}
