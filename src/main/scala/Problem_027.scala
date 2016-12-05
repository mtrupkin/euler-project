/**
  * Created by miketrupkin on 12/5/16.
  */
object Problem_027 extends App {
  import Primes._

  def quadratic(a: Int, b: Int): Int => Int = { n => n * n + a * n + b }

  def consecutivePrimes(f: Int => Int): Int = {
    Stream.from(0).map(f).takeWhile(_.isPrime).length
  }

  def generate(): Seq[(Int, Int, Int)]  = {
    for {
      a <- -999 to 999
      b <- -1000 to 1000
    } yield (consecutivePrimes(quadratic(a, b)), a, b)
  }

  val (count, a, b) = generate().maxBy( _._1 )

  println(s"count: $count a: $a b: $b product: ${a * b}")

  println(consecutivePrimes(quadratic(-79, 1601)))



}
