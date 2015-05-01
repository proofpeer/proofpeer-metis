package proofpeer.metis.util
import scala.collection.Set
import scala.collection.IterableLike
import scala.collection.GenTraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scalaz._
import Scalaz._

class RichFoldable[F[_]:Foldable,A](xs: F[A])(implicit ev:ApplicativePlus[F]) {
  def distinctBy(p: (A,A) => Boolean) = {
    xs.foldLeft((ev.monoid[A].zero,Set[A]())) {
      case ((acc,dups),x) =>
        if (dups.exists(y => p(x,y)))
          (acc,dups)
        else (ev.monoid.append(acc,ev.point(x)), dups + x)
    }
  }
}

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
}

class RichTraversable[A, That, This <: GenTraversableLike[A,That]](
  xs: GenTraversableLike[A,This]) {
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
  implicit def toRichFoldable[F[_]:Foldable,A](xs: F[A])(
    implicit ev:ApplicativePlus[F]) {
    new RichFoldable(xs)
  }
  implicit def toRichGenTraversable[A,That,This <: GenTraversableLike[A,That]](
    xs: GenTraversableLike[A,This]) =
    new RichTraversable[A,That,This](xs)
  implicit def setToRichTraversable[A](xs: Set[A]) =
    new RichTraversable[A,Set[A],Set[A]](xs)
  implicit def toRichOption[A](x: Option[A]) = new RichOption(x)
  // TODO: Add listToRichTraversable and replace uses of headOption to take a singleton element with singleton. Either that, or figure out why you decided to have patMatch and unify return lists rather than options.
}
