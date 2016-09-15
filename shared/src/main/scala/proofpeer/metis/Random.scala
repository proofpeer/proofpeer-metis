// package proofpeer.metis

// import scala.language.higherKinds
// import scalaz._
// import Scalaz._

// /** Metis' random number generator. Provided for consistency in testing, and because
//   * I want pure functional RNGs.
//   */
// object MetisRNG {
//   type M[A] = State[Long,A]

//   val maxLong = 0x3FFFFFFF
//   val topBit  = 0x20000000

//   def step(seed: Long) = {
//     (0x2E523085 * seed + 1) & maxLong
//   }

//   private def nextWord = {
//     for (
//       seed <- get[Long];
//       next = step(seed);
//       _    <- put[Long](next))
//     yield next
//   }

//   def nextInt(n: Int) = {
//     if (n <= 0 || n > maxLong) {
//       throw new IllegalArgumentException("Integer out of range")
//     }

//     nextWord.map(w => (w % n).toInt)
//   }

//   def nextBoolean = {
//     nextWord.map(n => (n & topBit) == 0)
//   }

//   def pickElement[A](xs: List[A]) = {
//     MetisRNG.nextInt(xs.length) map (xs(_))
//   }
// }
