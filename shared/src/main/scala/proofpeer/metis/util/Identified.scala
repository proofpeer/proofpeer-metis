package proofpeer.metis.util

import java.util.ArrayList

import scalaz._
import Scalaz._

class Identified[A:Order] {
  private var i = 0
  private var m = IMap.empty[A,Int]
  private var objs = new ArrayList[A]()
  def getIndex(x: A): Int =
    m.lookup(x).getOrElse {
      m = m.insert(x,i)
      objs.add(x)
      i = i + 1
      i
    }
  def getById(i: Int): A = objs.get(i)
}

