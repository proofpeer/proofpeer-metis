package proofpeer.metis.util
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Set

class RichCollection[A, This](xs: IterableLike[A,This]) {
  def distinctBy[That](p: (A,A) => Boolean)(implicit
    bf: CanBuildFrom[This,A,That]) = {
    val builder = bf(xs.repr)
    val dups = Set[A]()
    for (x <- xs) {
      if (!dups.exists(y => p(x,y))) {
        builder += x
        dups += x
      }
    }
    builder.result
  }
}

import scala.language.implicitConversions
object RichCollectionInstances {
  implicit def toRich[A,This](xs: IterableLike[A,This]) = new RichCollection(xs)
}
