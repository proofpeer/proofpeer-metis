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

  /** A generic unfold. */
  def unfoldM[M[_]:Monad,W:Monoid,A](x: A)(f: A => M[Option[(A,W)]]): M[W] = {
    f(x) >>= {
      case None          => ∅[W].point[M]
      case (Some((y,w))) => unfoldM(y)(f).map {w ⊹ _}
    }
  }

  /** A generic unfold */
  def unfold[W:Monoid,A](x: A)(f: A => Option[(A,W)]): W =
    unfoldM[Id,W,A](x)(f)

  /** Options are monoids favouring the right. */
  def RightBias[A] = new Monoid[Option[A]] {
    def zero = None
    def append(x: Option[A], y: => Option[A]) =
      (x,y) match {
        case (x,None) => x
        case (_,y)    => y
      }
  }

  /** Try to loop at least once, returning the last defined element. */
  def loopM1[M[_]:Monad,A](x: A)(f : A => M[Option[A]]): M[Option[A]] = {
    implicit val RightBiasMonoid = RightBias[A]
    unfoldM[M,Option[A],A](x) { x => f(x).map(_.map { y => (y, Some(y)) }) }
  }

  /** Try to loop at least once, returning the last defined element. */
  def loop1[A](x:A)(f: A => Option[A]): Option[A] =
    loopM1[Id,A](x)(f)

  def loopM[M[_]:Monad,A](x:A)(f: A => M[Option[A]]): M[A] =
    loopM1(x)(f).map(_.getOrElse(x))

  def loop[A](x:A)(f: A => Option[A]): A = loopM[Id,A](x)(f)

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
