/**
  * Created by miketrupkin on 11/29/16.
  */
object Problem_046 extends App {
  def isPrime(n: Int) = (2 until Math.sqrt(n).toInt) forall (n % _ != 0)

  def isPerfectSquare(n: Double): Boolean = {
    val sqrt = Math.sqrt(n).toLong
    sqrt*sqrt == n
  }

  def goldbachTest(guess: Int, prime: Int): Boolean = {
    isPerfectSquare( (guess - prime) / 2.0 )
  }

  def goldbachCounterExample(): Int = {
    val q = Stream.from(5, 2).takeWhile( guess => {
      if (isPrime(guess)) true else {
        val q2 = for {
          prime <- Primes.primes_.takeWhile(_ < guess)
        } yield goldbachTest(guess, prime.toInt)

        val r = q2.forall(!_)
        if (r) {
          println(s"guess: $guess")
          println(q2)
        }

        !r
      }
    })
    println(q.toList)
    q.last
  }

  println(goldbachCounterExample())
}
