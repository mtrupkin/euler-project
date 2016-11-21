import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
  * Created by miketrupkin on 11/21/16.
  */
object Primes extends App {
  // returns primes from 2 to n
  def primesFunctional(n: Long): List[Long] = {
    def primes(ps: List[Long], ns: List[Long]): List[Long] = {
      // remove composite numbers from candidates
      val ns2 = ns.filter(_ % ps.head != 0)
      ns2 match {
        case nextPrime :: _ => primes(nextPrime :: ps, ns2)
        case Nil => ps
      }
    }

    primes(List(2), Range.Long(3L, n, 2L).toList)
  }

  def apply(end: Int): List[Long] = {
    val primeIndices: ArrayBuffer[Long] = mutable.ArrayBuffer.fill((end + 1) / 2)(1L)

    val intSqrt = Math.sqrt(end).toInt
    for (i <- 3 to end by 2 if i <= intSqrt) {
      for (nonPrime <- i * i to end by 2 * i) {
        primeIndices.update(nonPrime / 2, 0)
      }
    }

    2 :: (for (i <- primeIndices.indices if primeIndices(i) == 1) yield 2 * i + 1L).tail.toList
  }
}
