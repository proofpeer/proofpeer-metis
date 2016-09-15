package proofpeer.metis.util
import scala.collection.IterableLike
import scala.collection.GenTraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scalaz._
import Scalaz._

class RichFoldable[F[_]:Foldable,A:Order](xs: F[A])(implicit ev:ApplicativePlus[F]) {
  def distinctBy(p: (A,A) => Boolean) = {
    xs.foldLeft((ev.monoid[A].zero,∅[ISet[A]])) {
      case ((acc,dups),x) =>
        if (dups.any(y => p(x,y)))
          (acc,dups)
        else (ev.monoid.append(acc,ev.point(x)), dups union ISet.singleton(x))
    }
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
  implicit def toRichFoldable[F[_]:Foldable,A:Order](xs: F[A])(
    implicit ev:ApplicativePlus[F]) {
    new RichFoldable(xs)
  }
  implicit def toRichOption[A](x: Option[A]) = new RichOption(x)
  // TODO: Add listToRichTraversable and replace uses of headOption to take a singleton element with singleton. Either that, or figure out why you decided to have patMatch and unify return lists rather than options.
}
