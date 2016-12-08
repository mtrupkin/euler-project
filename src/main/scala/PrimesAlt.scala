/**
  * Created by miketrupkin on 12/8/16.
  */
object PrimesAlt {
  import Primes.primes_

  def primeFactors2(composite: Long): Stream[Long] = {
    def primeFactors(n: Long, remainingPrimes: List[Long], acc: Stream[Long]): Stream[Long] = {
      val factor = remainingPrimes.head

      if (factor * factor > n) return n #:: acc

      if (n % factor == 0)
        primeFactors(n / factor, remainingPrimes, factor #:: acc)
      else primeFactors(n, remainingPrimes.tail, acc)
    }

    //    primeFactors(composite, Nil, primes(Math.sqrt(composite).toInt))
    primeFactors(composite, primes_, Stream.empty)
  }

  def primeFactorsSet(composite: Long): Set[Long] = {
    def primeFactors(n: Long, acc: Set[Long], remainingPrimes: List[Long]): Set[Long] = {
      if (n == 1) return acc

      val factor = remainingPrimes.head
      if (factor * factor > composite) return if (n % factor == 0) acc + factor else acc

      if (n % factor == 0)
        primeFactors(n / factor, acc + factor, remainingPrimes.tail)
      else primeFactors(n, acc, remainingPrimes.tail)
    }

    //    primeFactors(composite, Nil, primes(Math.sqrt(composite).toInt))
    primeFactors(composite, Set.empty, primes_)
  }

  def primeFactorsStream(composite: Long): Stream[Long] = {
    def primeFactors(n: Long, acc: => Stream[Long], remainingPrimes: List[Long]): Stream[Long] = {
      if (n == 1) return acc

      val factor = remainingPrimes.head
      if (factor * 2 > composite) return acc

      if (n % factor == 0)
        primeFactors(n / factor, factor #:: acc, remainingPrimes)
      else primeFactors(n, acc, remainingPrimes.tail)
    }

    //    primeFactors(composite, Nil, primes(Math.sqrt(composite).toInt))
    primeFactors(composite, Stream.empty, primes_)
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
}
