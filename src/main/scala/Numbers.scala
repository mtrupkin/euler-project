import scala.io.Source

/**
  * Created by miketrupkin on 11/15/16.
  */
object Numbers extends App {
  def toDigits(n: Int): List[Int] = n.toString.map(_.asDigit).toList

  // returns primes from 2 to n
  def primes(n: Long): List[Long] = {
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


  lazy val primes_ : List[Long] = {
    val is = getClass.getResourceAsStream("primes.txt")
    Source.fromInputStream(is).getLines().map(_.toLong).toList
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


  println(Numbers.primeFactors(600851475143L))
}
