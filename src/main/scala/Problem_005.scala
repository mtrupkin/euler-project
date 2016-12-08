/**
  * Smallest positive number that is evenly divisible by all of the numbers from 1 to 20
  *
  * 9699690 is the product of all primes less than 20
  */
object Problem_005 extends App {

  // test if number is evenly divisible by all of the numbers from 1 to 20
  def isMultiple(n: Long): Boolean = (2 to 20).forall(n % _ == 0)

  // find the first number (1 to 20) that is not a multiple of n
  def missingMultiple(n: Long): Long = (2 to 20).find(n % _ != 0).get

  // find the prime factor of the first missing multiple m
  def missingPrimeFactor(n: Long): Long = Primes.primeFactors(missingMultiple(n)).head

  // list all numbers (1 to 20) that is not a multiple of n
  // Note: not used in solution, just a test
  def missingMultiples(n: Long): List[Long] = {
    (2 to 20).filter(n % _ != 0).map(_.toLong).toList
  }

  // smallest number that is evenly divisible by all of the prime numbers below 20
  val n = Primes(20).product

  def searchMultiple(n: Long): Long = {
    if (isMultiple(n)) n else searchMultiple(n*missingPrimeFactor(n))
  }

  println(searchMultiple(n))
}
