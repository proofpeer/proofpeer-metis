package proofpeer.metis.fol

import proofpeer.metis. { Pred => MPred, _ }
import scala.language.higherKinds
import scalaz._
import Scalaz._
import Prov._
import TermInstances._
import LiteralInstances._
import ClauseInstances._

/** First order term.
  * @tparam V Type of variables
  * @tparam F Type of functors
  * @tparam P Type of predicate-functors
  * @tparam U Type of unary connectives (either Neg or Nothing)
  * @tparam B Type of binders (either Binder or Nothing)
  */
sealed abstract class FOL[V,F,P,+U,+B]
case class Pred[V,F,P,U,B](p: P, args: List[Term[V,F]])     extends FOL[V,F,P,U,B]
case class And[V,F,P,U,B](
  p: FOL[V,F,P,U,B], q: FOL[V,F,P,U,B])                     extends FOL[V,F,P,U,B]
case class Or[V,F,P,U,B](
  p: FOL[V,F,P,U,B], q: FOL[V,F,P,U,B])                     extends FOL[V,F,P,U,B]
case class Unary[V,F,P,U,B](u: U, p: FOL[V,F,P,U,B])        extends FOL[V,F,P,U,B]
case class Bnding[V,F,P,U,B](b: B, v: V, p: FOL[V,F,P,U,B]) extends FOL[V,F,P,U,B]

object FOL {
  sealed abstract class Binder
  case object All    extends Binder
  case object Exists extends Binder

  sealed case class Neg()

  /** Freshened variables are integers tracking their original. */
  type Fresh[V] = Prov[V,Int]

  /** FOL terms are functors in their (syntactic) functors. */
  def trimap[V,F,P,V_,F_,P_,U,B](fol: FOL[V,F,P,U,B])(
    f: V => V_, g: F => F_, h: P => P_): FOL[V_,F_,P_,U,B] = {
    fol match {
      case Pred(p,args)  => Pred(h(p),args.map(_.bimap(f,g)))
      case And(p,q)      => And(trimap(p)(f,g,h),trimap(q)(f,g,h))
      case Or(p,q)       => Or(trimap(p)(f,g,h),trimap(q)(f,g,h))
      case Unary(u,p)    => Unary(u,trimap(p)(f,g,h))
      case Bnding(b,v,p) => Bnding(b,f(v),trimap(p)(f,g,h))
    }
  }

  object Instances {
    implicit def FOLIsFunctor[F,P,Un,B]: Functor[({type λ[V] = FOL[V,F,P,Un,B]})#λ] =
    new Functor[({type λ[V] = FOL[V,F,P,Un,B]})#λ] {
      override def map[U,V](fol: FOL[U,F,P,Un,B])(f: U => V): FOL[V,F,P,Un,B] = {
        fol match {
          case Pred(p,args)    => Pred(p,args.map(_.map(f)))
          case And(p,q)        => And(map(p)(f),map(q)(f))
          case Or(p,q)         => Or(map(p)(f),map(q)(f))
          case Unary(u,p)      => Unary(u,map(p)(f))
          case Bnding(bnd,v,p) => Bnding(bnd,f(v),map(p)(f))
        }
      }
    }

    implicit def PredIsOrder[V:Order,F:Order,P:Order]:
        Order[Pred[V,F,P,Nothing,Nothing]] =
      new Order[Pred[V,F,P,Nothing,Nothing]] {
        override def order(
          x: Pred[V,F,P,Nothing,Nothing],
          y: Pred[V,F,P,Nothing,Nothing]) = {
          val Pred(p1, args1) = x
          val Pred(p2, args2) = y
          (p1 ?|? p2) |+| (args1 ?|? args2)
        }
      }

    implicit def FOLIsOrder[V:Order,F:Order,P:Order,U:Order,B:Order]:
        Order[FOL[V,F,P,U,B]] =
      new Order[FOL[V,F,P,U,B]] {
        override def order(x: FOL[V,F,P,U,B], y: FOL[V,F,P,U,B]) =
          (x,y) match {
            case (Pred(p1, args1), Pred(p2, args2)) =>
              (p1 ?|? p2) |+| (args1 ?|? args2)
            case (Pred(_,_), _) => Ordering.LT
            case (_, Pred(_,_)) => Ordering.GT
            case (And(p1,q1), And(p2,q2)) => (p1 ?|? p2) |+| (q1 ?|? q2)
            case (And(_,_), _) => Ordering.LT
            case (_, And(_,_)) => Ordering.GT
            case (Or(p1,q1), Or(p2,q2)) => (p1 ?|? p2) |+| (q1 ?|? q2)
            case (Or(_,_), _) => Ordering.LT
            case (_, Or(_,_)) => Ordering.GT
            case (Unary(u,p), Unary(v,q)) => (u ?|? v) |+| (p ?|? q)
            case (Unary(_,_), _) => Ordering.LT
            case (_, Unary(_,_)) => Ordering.GT
            case (Bnding(b1,v1,p1), Bnding(b2,v2,p2)) =>
              (b1 ?|? b2) |+| (v1 ?|? v2) |+| (p1 ?|? p2)
            case (Bnding(_,_,_), _) => Ordering.LT
            case (_, Bnding(_,_,_)) => Ordering.GT
          }
    }

    implicit def NegIsShow = new Show[Neg] {
      override def show(neg: Neg) = Cord("~")
    }

    implicit def NegIsOrder = new Order[Neg] {
      override def order(x:Neg,y:Neg) = Ordering.EQ
    }
    implicit def BindingIsOrder = new Order[Binder] {
      override def order(x:Binder,y:Binder) = (x,y) match {
        case (Exists, Exists) => Ordering.EQ
        case (Exists, All) => Ordering.LT
        case (All, Exists) => Ordering.GT
        case (All, All) => Ordering.EQ
      }
    }
  }

  def toNNF[V,F,P,U](fol: FOL[V,F,P,Neg,Binder]):
      FOL[V,F,(Option[Neg],P),U,Binder] = {
    fol match {
      case Pred(p,args)         => Pred((None,p),args)
      case And(p,q)             => And(toNNF(p),toNNF(q))
      case Or(p,q)              => Or(toNNF(p),toNNF(q))
      case Unary(Neg(),p)       => notnnf(p)
      case Bnding(All,v,bod)    => Bnding(All,v,toNNF(bod))
      case Bnding(Exists,v,bod) => Bnding(Exists,v,toNNF(bod))
    }
  }

  private def notnnf[V,F,P,U](fol: FOL[V,F,P,Neg,Binder]):
      FOL[V,F,(Option[Neg],P),U,Binder] = {
    fol match {
      case Pred(p,args)         => Pred((Some(Neg()),p),args)
      case And(p,q)             => Or(notnnf(p),notnnf(q))
      case Or(p,q)              => And(notnnf(p),notnnf(q))
      case Unary(Neg(),p)       => toNNF(p)
      case Bnding(All,v,bod)    => Bnding(Exists,v,notnnf(bod))
      case Bnding(Exists,v,bod) => Bnding(All,v,notnnf(bod))
    }
  }
}

object Matrix {
  /** A matrix has no unary operators nor any binders. */
  type Matrix[V,F,P] = FOL[V,F,P,Nothing,Nothing]

  import FOL.{Binder,All,Exists,Neg,Instances}
  import Instances._

  /** All variables in a matrix. */
  def frees[V:Order,F:Order,P:Order](fol: Matrix[V,F,P]): ISet[V] =
    preds(fol).foldMap { case Pred(p,args) => args.foldMap(_.frees) }

  /** All predicates in a matrix. */
  def preds[V:Order,F:Order,P:Order](fol: Matrix[V,F,P]):
      ISet[Pred[V,F,P,Nothing,Nothing]] = {
    fol match {
      case p@Pred(_,_)      => ISet.singleton(p)
      case And(p,q)         => preds(p) |+| preds(q)
      case Or(p,q)          => preds(p) |+| preds(q)
      case Unary(void,_)    => void
      case Bnding(void,_,_) => void
    }
  }

  import FOL.Fresh
  /** Given a formula in NNF form (no unary operator), extract all binders,
      freshening variables as necessary.
    */
  def quantPull[V:Order,F,U,P](fol: FOL[V,F,P,Nothing,Binder]):
      (List[(Binder,Fresh[V])],
        Matrix[V \/ Fresh[V],F,P],
        FOL[V \/ Fresh[V],F,P,Nothing,Binder]) = {
    type QPS[A]     = State[(Int,V ==>> Fresh[V]),A]
    type WT[M[_],A] = WriterT[M,List[(Binder,Fresh[V])],A]
    type QPM[A]     = WT[QPS,A]
    type FOLIn  = FOL[V,F,P,Nothing,Binder]
    type FOLOut = FOL[V \/ Fresh[V],F,P,Nothing,Binder]
    type Mat_   = Matrix[V \/ Fresh[V],F,P]

    def getSubst : QPM[(V ==>> Fresh[V])] =
      get[(Int,V ==>> Fresh[V])].map(_._2).liftM[WT]

    def bindFresh(v: V): QPM[Fresh[V]] = {
      for (
        nθ    <- get[(Int,V ==>> Fresh[V])].liftM[WT];
        (n,θ) =  nθ;
        fresh =  originate(v).map(_ => n);
        _  <- put((n+1, θ + (v → Prov.originate(v).map {_ => n}))).liftM[WT])
      yield fresh
    }

    def qp(fol: FOLIn): QPM[(Mat_,FOLOut)] = {
      fol match {
        // Ugh. This is just biapplicative
        case And(p,q)      => for (
          qpp <- qp(p);
          qpq <- qp(q);
          (pmatrix,pfol) = qpp;
          (qmatrix,qfol) = qpq)
        yield (And(pmatrix,qmatrix),And(pfol,qfol))
        case Or(p,q)       => for (
          qpp <- qp(p);
          qpq <- qp(q);
          (pmatrix,pfol) = qpp;
          (qmatrix,qfol) = qpq)
        yield (Or(pmatrix,qmatrix),Or(pfol,qfol))
        case Unary(void,_) => void
        case Bnding(b,v,p) => bindFresh(v) >>= {
          v:Fresh[V] => List((b,v)) <++: qp(p)
          }
        case pred@Pred(p,args) =>
          getSubst map { θ =>
            val newargs = args.map(_.map(v =>
              θ.lookup(v).map(_.right).getOrElse(v.left)))
            (Pred(p,newargs),Pred(p,newargs))
          }
      }
    }
    val (bnds,(matrix,nfol)) = qp(fol).run.eval((0,==>>.empty))
    (bnds,matrix,nfol)
  }

  /** Given a binding list and its matrix, skolemise to a matrix which is implicitly
      universally quantified. */
  def skolemize[V:Order,F:Order,P:Order](
    binders: List[(Binder,V)],
    fol:     Matrix[V,F,P]): Matrix[V,V \/ F,P] = {

    val vsets = preds(fol).map { case Pred(_,args) => args.foldMap(_.frees) }
    val initSets = frees(fol).foldMap { v => ==>>.singleton(v,ISet.singleton(v)) }

    def groupSet(varSets: V ==>> ISet[V], vs: ISet[V]) = {
      val union  = vs.foldMap (v => varSets.lookup(v).get)
      val update = vs.foldMap { v => ==>>.singleton(v, union) }
      varSets |+| update
    }

    val groups = vsets.foldLeft(initSets)(groupSet)

    val (_,θ) = binders.foldLeft((List[V](),(==>>.empty[V,Term[V,V \/ F]]))) {
      case ((deps,θ),(All,v))    => (deps :+ v,θ)
      case ((deps,θ),(Exists,v)) =>
        val neededDeps = for (
          u <- deps;
          if groups.lookup(v).get.contains(u))
        yield Var[V,V \/ F](u)
        (deps, θ + (v → Fun(v.left,neededDeps)))
    }
    def applySubst(fol: Matrix[V,F,P]): Matrix[V,V \/ F,P] =
      fol match {
        case Pred(p,args)     => Pred(p,args.map { _.bimap(
          { v => θ.lookup(v).getOrElse(Var[V,V \/ F](v)) },
          {_.right[V]}).
            join
        })
        case And(p,q)         => And(applySubst(p),applySubst(q))
        case Or(p,q)          => Or(applySubst(p),applySubst(q))
        case Unary(void,_)    => void
        case Bnding(void,_,_) => void
      }

    applySubst(fol)
  }

  def showNNFMatrix[V:Show,F:Show,P:Show](fol: Matrix[V,F,(Option[Neg],P)]): Cord = {
    fol match {
      case Pred((neg,p),args) => (neg match {
        case Some(FOL.Neg())    => Cord("~")
        case None           => ∅[Cord]
      }) ++ p.show ++ Cord("(") ++ Cord.mkCord(",",args.map(_.show):_*) ++ Cord(")")
      case And(p,q)         => showNNFMatrix(p) ++ " ∧ " ++ showNNFMatrix(q)
      case Or(p,q)          => showNNFMatrix(p) ++ " ∨ " ++ showNNFMatrix(q)
      case Unary(void,_)    => void
      case Bnding(void,_,_) => void
    }
  }

  def showQuants[V:Show](quants: List[(FOL.Binder,V)]) =
    Cord.mkCord(" ",quants.map {
      case (All,v)    => Cord("∀") ++ v.show
      case (Exists,v) => Cord("∃") ++ v.show
    }:_*)
}

object CNF {
  import Matrix.Matrix
  import FOL.Neg
  def cnf_[V:Order,F:Order,P:Order](fol: Matrix[V,F,(Option[Neg],P)]):
      ISet[ISet[Literal[V,F,P]]] = {
    fol match {
      case Pred((None,p),args)        =>
        ISet.singleton(ISet.singleton(Literal(true,MPred(p,args))))
      case Pred((Some(Neg()),p),args) =>
        ISet.singleton(ISet.singleton(Literal(false,MPred(p,args))))
      case And(p,q)                   => cnf_(p) |+| cnf_(q)
      case Or(p,q)                    =>
        cnf_(p).foldMap { ps => cnf_(q).map { qs => ps |+| qs } }
      case Unary(void,_)              => void
      case Bnding(void,_,_)           => void
    }
  }

  /** Convert an implicitly universally quantified matrix into CNF form. */
  def cnf[V:Order,F:Order,P:Order](fol: Matrix[V,F,(Option[Neg],P)]):
      ISet[Clause[V,F,P]] =
    cnf_(fol).map(Clause(_))
}
