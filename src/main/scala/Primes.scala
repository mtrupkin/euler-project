import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
  * Prime numbers and utilities
  */
object Primes extends App {
  lazy val primes_ = this(1000000)

  implicit class PrimeDetector[B](n: B) {
    def isPrime(implicit num: Integral[B]): Boolean = {
      primes_.takeWhile(_ <= num.toLong(n)).contains(n)
    }
  }

  // returns prime factors of n
  def primeFactors(composite: Long): Stream[Long] = {
    def primeFactors(n: Long, factors: => Stream[Long], remainingPrimes: Seq[Long]): Stream[Long] = {
      val factor = remainingPrimes.head
      if (factor * factor > n) return n #:: factors

      if (n % factor == 0)
        primeFactors(n / factor, factor #:: factors, remainingPrimes)
      else primeFactors(n, factors, remainingPrimes.tail)
    }

    primeFactors(composite, Stream.empty, primes_)
  }

  // fast, mutable prime number generator
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
