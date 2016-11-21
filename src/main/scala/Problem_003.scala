import java.io.PrintWriter

import scala.io.Source

/**
  *  Problem - 3
  *
  *  Find the largest prime factor of 600851475143
  */
object Problem_003 extends App {
  // returns prime factors of n
  def primeFactors(n: Long, acc: List[Long], primes: List[Long]): List[Long] = {
    val factor = primes.find( n % _ == 0)

    factor match {
      case Some(f) => primeFactors(n/f, f :: acc, primes.dropWhile( _ != f))
      case None => acc
    }
  }

  def apply(n: Long): Long = {
    val primes = Primes(Math.sqrt(n).toInt)
    val fs = primeFactors(n, Nil, primes)
    fs.head
  }

  println(Problem_003(600851475143L))
}
