package proofpeer.metis.testing
import scala.language.implicitConversions
import scalaz._
import Scalaz._

import org.scalatest.{FlatSpec}
import proofpeer.metis._

class TermSpec extends FlatSpec {

  val termParsers = new TermParsers()
  def parseTerm(str: String) = {
    val tokens = new termParsers.lexical.Scanner(str)
    termParsers.parseTerm(tokens)
  }
  def parseAtom(str: String) = {
    val tokens = new termParsers.lexical.Scanner(str)
    termParsers.parseAtom(tokens)
  }
  def parseLit(str: String) = {
    val tokens = new termParsers.lexical.Scanner(str)
    termParsers.parseLit(tokens)
  }

  val kernel = new Kernel[String,String,String]
  type StringTerm = Term[String,String]
  type StringAtom = Atom[String,String,String]
  type StringLit = Literal[String,String,String]
  type QSubst = PartialFunction[String,Term[Unit,(String,Int)]]
  type Conv = StringTerm => Option[(StringTerm,kernel.Thm)]
  type TermNet = Nets.TermNet[String, Conv]

  import ClauseInstances._

  // Remember that Term.termMatch overmatches. Should we check in the conversion and
  // return None as appropriate, or let this be handled in repeatTopDownConvRule?
  // I'd prefer the former for now, even if it leads to duplicate code. I'd rather
  // enforce the rule that invalid conversions do not silently fail, but are
  // regarded as errors.
  def addRewrite(eq: kernel.Thm, net: TermNet) = {
    val Literal(true,Eql(lhs,rhs)) = eq.clause.filter {
      case Literal(true,Eql(_,_)) => true
      case _ => false
    }.head
    net.insert(lhs, { tm =>
      lhs.patMatch(Subst.empty, tm).headOption.map { θ =>
        (rhs.subst(θ),eq.subst(θ)) }
    })
  }

  def rewrite(net: TermNet, lit: StringLit) = {
    kernel.repeatTopDownConvRule(
      lit,
      { tm => net.matches(tm).map {_.apply(tm)}.flatten.headOption })
  }

  def concatStrings(string: List[String]) = {
    string.foldLeft(new StringBuilder()) {
      (sb, str) => sb.append(str)
    }.result
  }

  def ppTerm(term: StringTerm): String = {
    term match {
      case Var(v) => v
      case Fun(f,List()) => f
      case Fun(f,args) => f + "(" + concatStrings(args.map(ppTerm(_)).intersperse(", ")) + ")"
    }
  }

  def ppAtom(atom: StringAtom): String = {
    atom match {
      case Eql(lhs,rhs) => ppTerm(lhs) + " = " + ppTerm(rhs)
      case Pred(p,List()) => p
      case Pred(p,args) => p + "(" + concatStrings(args.map(ppTerm(_)).intersperse(", ")) + ")"
    }
  }

  def ppLit(lit: StringLit): String = {
    (if (!lit.isPositive)
      "~"
    else "") + ppAtom(lit.atom)
  }

  def ppThm(thm: kernel.Thm): String = {
    concatStrings(thm.clause.lits.toList.map(ppLit(_)).intersperse("\n ∨ "))
  }

  var rewrNet:TermNet = new Nets.TermNet()
  rewrNet = addRewrite(kernel.axiom(Clause(Set(
    parseLit("Plus(x,Zero) = x").get,
    parseLit("~PlusId").get))),
    rewrNet)
  rewrNet = addRewrite(kernel.axiom(Clause(Set(
    parseLit("Times(x,Zero) = Zero").get,
    parseLit("~TimesZero").get))),
    rewrNet)
  rewrNet = addRewrite(kernel.axiom(Clause(Set(
    parseLit("Times(x,One) = x").get,
    parseLit("~TimesId").get))),
    rewrNet)

  val lit1 = parseLit("P(Plus(x,Times(y,Times(Zero,One))))").get
  val rewrLit1 = rewrite(rewrNet,lit1)
}
