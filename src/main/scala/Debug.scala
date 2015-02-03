package proofpeer.metis

object Debug {
  def stringClause[V,F,P](lits:Set[Literal[V,F,P]]) = {
    lits.map { case lit:Literal[String,String,String] =>
      TermPrinter.printLiteral(lit)
    }
  }

  def stringTerm[V,F,P](term:Term[V,F]) = {
    term match {
      case tm: Term[String,String] => TermPrinter.printTerm(tm)
    }
  }

  def stringLiteral[V,F,P](lit:Literal[V,F,P]) = {
    lit match {
      case lit: Literal[String,String,String] => TermPrinter.printLiteral(lit)
    }
  }

  def stringAtom[V,F,P](lit:Atom[V,F,P]) = {
    lit match {
      case lit: Atom[String,String,String] => TermPrinter.printAtom(lit)
    }
  }

  def printClause[V,F,P](lits:Set[Literal[V,F,P]]) = {
    lits.foreach { case lit:Literal[String,String,String] =>
      System.out.println(TermPrinter.printLiteral(lit))
    }
    System.out.println("")
  }
}
