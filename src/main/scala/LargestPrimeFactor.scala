import java.io.PrintWriter

import scala.io.Source

/**
  *  Problem - 3
  *
  *  Find the largest prime factor
  */
object LargestPrimeFactor extends App {

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

    primes(List(2), (3L to n).toList)
  }

  // returns prime factors of n
  def factors(n: Long, acc: List[Long], primes: List[Long]): List[Long] = {
    val factor = primes.find( n % _ == 0)

    factor match {
      case Some(f) => factors(n/f, f :: acc, primes.dropWhile( _ != f))
      case None => acc
    }
  }

  def apply(n: Long): Long = {
    val ps = readPrimes()
    val fs = factors(n, Nil, ps)
    fs.head
  }

  def writePrimes(n: Long): Unit = {
    val ps = primes(n).reverse
    val pw = new PrintWriter("primes.txt")
    ps.foreach(pw.println(_))
    pw.close()
  }

  def readPrimes(): List[Long] = {
    val is = getClass.getResourceAsStream("primes.txt")
    Source.fromInputStream(is).getLines().map(_.toLong).toList
  }

  println(LargestPrimeFactor(600851475143L))
}
