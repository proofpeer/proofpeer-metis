package proofpeer.metis.util
import scala.collection.Set
import scala.collection.IterableLike
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

class RichIterable[A, This](xs: IterableLike[A,This]) {
  def distinctBy[That](p: (A,A) => Boolean)(implicit
    bf: CanBuildFrom[This,A,That]) = {
    val builder = bf(xs.repr)
    val dups = scala.collection.mutable.Set[A]()
    for (x <- xs) {
      if (!dups.exists(y => p(x,y))) {
        builder += x
        dups += x
      }
    }
    builder.result
  }

  def foldLeftOpt[B](b:B)(f: (B,A) => Option[B]) = {
    xs.foldLeft(b) {
      case (acc,x) => f(acc,x).getOrElse(acc)
    }
  }
}

class RichTraversable[A, That, This <: TraversableLike[A,That]](
  xs: TraversableLike[A,This]) {
  def singleton = {
    val (head,tail) = xs.splitAt(1)
    head.headOption.filter(_ => tail.isEmpty)
  }
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
  implicit def toRichIterable[A,This](xs: IterableLike[A,This]) =
    new RichIterable(xs)
  implicit def toRichTraversable[A,That,This <: TraversableLike[A,That]](
    xs: TraversableLike[A,This]) =
    new RichTraversable[A,That,This](xs)
  implicit def setToRichTraversable[A](xs: Set[A]) =
    new RichTraversable[A,Set[A],Set[A]](xs)
  implicit def toRichOption[A](x: Option[A]) = new RichOption(x)
  // TODO: Add listToRichTraversable and replace uses of headOption to take a singleton element with singleton. Either that, or figure out why you decided to have patMatch and unify return lists rather than options.
}
