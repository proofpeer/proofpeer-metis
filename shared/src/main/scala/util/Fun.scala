package proofpeer.metis.util

import scala.language.higherKinds
import scalaz._
import Scalaz._

object Fun {
  def splits[A](xs: List[A]) = {
    def f(pre: List[A], xs: List[A], acc: List[(List[A],A,List[A])]):
        List[(List[A],A,List[A])] = {
      xs match {
        case List() => acc
        case y::ys  => f(y::pre, ys, (pre,y,ys)::acc)
      }
    }
    f(List(), xs, List())
  }

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
  def unfoldW[W:Monoid,A](x: A)(f: A => Option[(A,W)]): W =
    unfoldM[Id,W,A](x)(f)

  /** Loop, collecting results. */
  def loopCollectM[M[_]:Monad,A](x: A)(f: A => M[Option[A]]): M[List[A]] =
    (unfoldM[M,DList[A],A](x) { x => f(x).map(_.map(x => (x,x +: ∅[DList[A]]))) })
      .map(_.toList)

  /** Loop, collecting results. */
  def loopCollect[A](x: A)(f: A => Option[A]): List[A] =
    (unfoldM[Id,DList[A],A](x) { x => f(x).map(x => (x,x +: ∅[DList[A]])) }).toList

  /** Loop at least once, returning the last defined element. */
  def loopM[M[_]:Monad, A](x: A)(f : A => M[Option[A]]): M[Option[A]] =
    unfoldM(x) { f(_).map (_.map { x => (x,Tags.Last(x.some)) }) }
      .map(Tags.Last.unwrap(_))

  /** Loop at least once, returning the last defined element. */
  def loop1[A](x: A)(f : A => Option[A]): Option[A] = {
    loopM[Id,A](x)(f)
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
