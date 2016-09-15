// package proofpeer.metis

// import scala.language.higherKinds
// import scalaz._
// import Scalaz._

// /** The positive integers 0 <= n < size, where size > 0. */
// case class FinOrd(size: Int) {
//   if (size <= 0) {
//     throw new IllegalArgumentException("size must be greater than 0.")
//   }

//   case class Fin(n: Int) {
//     /** Returns the successor, if there is one. */
//     def suc = {
//       val s = this.n + 1
//       if (s < size) Some(Fin(s)) else None
//     }
//   }

//   val zero = Fin(0)

//   /** Returns n % size. */
//   def modInt(n: Int) = {
//     Fin(n % size)
//   }

//   /** The random member of the set. */
//   def random = {
//     MetisRNG.nextInt(size).map{Fin(_)}
//   }

//   /** Wrap lists of Fins, each with a unique hash. */
//   case class ListFin(ns: List[Fin]) {
//     override def hashCode() = {
//       ns.foldLeft(0) {
//         case (acc,n) => acc * size + n.n
//       }
//     }
//   }

//   def filter(p: Fin => Boolean) = {
//     for (
//       i <- 0 to (size-1);
//       f = Fin(i);
//       if p(f))
//     yield f
//   }

//   def filterM[M[_]: Monad](p: Fin => M[Boolean]): M[List[Fin]] = {
//      (0 to (size-1)).toList.map(Fin(_)).filterM(p)
//   }

//   implicit val ordFin: Order[Fin] =
//     new Order[Fin]{
//       def order (f1:Fin, f2:Fin): Ordering = f1.n.cmp(f2.n)
//     }
// }
