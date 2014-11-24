package proofpeer.metis.util

object Fun {
  def repeat[A](f: A => Option[A], x: A): A = {
    f(x) match {
      case None    => x
        case Some(y) => repeat(f,y)
    }
  }
}
