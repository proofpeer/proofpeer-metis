package proofpeer.metis.util
import scala.collection.IterableLike
import scala.collection.GenTraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scalaz._
import Scalaz._

object ISetExtra {
  def distinctBy[A:Order](xs: ISet[A])(p: (A,A) => Boolean): ISet[A] = {
    xs.foldLeft((∅[ISet[A]],∅[ISet[A]])) {
      case ((acc,dups),x) =>
        if (dups.any(y => p(x,y)))
          (acc,dups)
        else (acc.insert(x), dups.insert(x))
    }._1
  }
}

class RichFoldable[F[_]:Foldable,A](xs: F[A]) {
  def getSingleton = xs.foldRight((none[A],true)) {
    case (x,(_,true)) => (Some(x),false)
    case (x,(_,false)) => (None,false)
  }._1
  def findFirst[B](f : A => Option[B]) =
    Tags.First.unwrap(xs.foldMap(x => Tags.First(f(x))))
}

class RichOption[A](x: Option[A]) {
  def getOrBug(errMsg: String) =
    x match {
      case None     => throw new Error("BUG: " + errMsg)
      case Some(x_) => x_
    }
}

import scala.language.implicitConversions
object RichCollectionInstances {
  implicit def toRichOption[A](x: Option[A]) = new RichOption(x)
  implicit def toRichFoldable[F[_]:Foldable,A](xs: F[A]) =
    new RichFoldable(xs)
}
