package proofpeer.metis.util

import scala.language.higherKinds
import scalaz._
import Scalaz._

object Fun {
  /** f (f (f .. x)) where f is repeated n times.
    Alternatively, the nth Church numeral. */
  def iterate[A](n: Int, x: A)(f: A => A) = {
    def loop(i: Int, acc: A): A = {
      if (i <= 0) acc else loop(i-1, f(acc))
    }
    loop(n, x)
  }

  /** f (return x) >>= f >>= f >>= ... >>= f where f is repeated n times. */
  def iterateM[M[_]:Monad,A](n: Int, x:A)(f: A => M[A]) = {
    def loop(i: Int)(acc: A): M[A] = {
      if (i <= 0) acc.point[M] else f(acc) >>= loop(i-1)
    }
    loop(n)(x)
  }

  def unfold[A](x: A)(f: A => Option[A]): A = {
    f(x) match {
      case None    => x
        case Some(y) => unfold(y)(f)
    }
  }

  def existsM[X,M[_]:Monad](xs: Iterator[X])(p: X => M[Boolean]): M[Boolean] = {
    var it = xs

    def loop: M[Boolean] = {
      if (it.isEmpty)
        false.point[M]
      else {
        val x = it.next
        p(x) >>= {
          case true   => true.point[M]
          case false  => loop
        }
      }
    }

    loop
  }
}
