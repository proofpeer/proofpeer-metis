package proofpeer.metis.util

import scalaz._

trait PartialOrder[A] {
  def tryCompare(x: A, y: A): Option[Ordering]
}
