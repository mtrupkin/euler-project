import scala.io.Source

/**
  * Created by miketrupkin on 11/14/16.
  */
object Primes {
  val primes_ = {
    val is = getClass.getResourceAsStream("primes.txt")
    Source.fromInputStream(is).getLines().map(_.toLong).toList
  }

  def apply(): List[Long] = primes_

  def isPrime(n: Int): Boolean = {
    if (n == 1) return false
    val sqrt = Math.sqrt(n)
    val res = primes_.takeWhile( _ <= sqrt).find(n % _ == 0)
    res.isEmpty
  }
}
