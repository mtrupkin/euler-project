import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
  * Prime numbers and utilities
  */
object Primes extends App {
  lazy val primes_ = this(100000)

  implicit class PrimeDetector[B](n: B) {
    def isPrime(implicit num: Integral[B]): Boolean = {
      primes_.takeWhile(_ <= num.toLong(n)).contains(n)
    }
  }


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

  // returns prime factors of n
  def primeFactors(composite: Long): List[Long] = {
    def primeFactors(n: Long, acc: List[Long], remainingPrimes: List[Long]): List[Long] = {
      if (n == 1) return acc

      val factor = remainingPrimes.head
      if (factor * 2 > composite) return acc

      if (n % factor == 0)
        primeFactors(n / factor, factor :: acc, remainingPrimes)
      else primeFactors(n, acc, remainingPrimes.tail)
    }

    //    primeFactors(composite, Nil, primes(Math.sqrt(composite).toInt))
    primeFactors(composite, Nil, primes_)
  }

  // fast, mutable prime number finder
  def apply(length: Int): List[Long] = {
    val primeIndices: ArrayBuffer[Long] = mutable.ArrayBuffer.fill((length + 1) / 2)(1L)

    val intSqrt = Math.sqrt(length).toInt
    for (i <- 3 to length by 2 if i <= intSqrt) {
      for (nonPrime <- i * i to length by 2 * i) {
        primeIndices.update(nonPrime / 2, 0)
      }
    }

    2 :: (for (i <- primeIndices.indices if primeIndices(i) == 1) yield 2 * i + 1L).tail.toList
  }
}
