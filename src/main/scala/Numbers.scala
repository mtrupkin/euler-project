import scala.io.Source

/**
  * Created by miketrupkin on 11/15/16.
  */
object Numbers extends App {
  def toDigits(n: Int): List[Int] = n.toString.map(_.asDigit).toList


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
