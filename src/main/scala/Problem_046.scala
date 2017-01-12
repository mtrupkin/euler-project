/**
  * Created by miketrupkin on 11/29/16.
  */
object Problem_046 extends App {
  def isPrime(n: Int) = (2 until Math.sqrt(n).toInt) forall (n % _ != 0)

  def isPerfectSquare(n: Double): Boolean = {
    val sqrt = Math.sqrt(n).toLong
    sqrt*sqrt == n
  }

  // solve Golbach test in terms of just the odd composite and a prime
  def goldbachTest(guess: Int, prime: Int): Boolean = {
    isPerfectSquare( (guess - prime) / 2.0 )
  }

  def goldbachCounterExample(): Int = {
    // all odd numbers starting with 5
    val q = Stream.from(5, 2).takeWhile( guess => {

      // check to stop processing, skipping prime numbers
      if (isPrime(guess)) true else {

        // Goldbach test results for each prime
        val goldbachTestResults = for {
          prime <- Primes.primes_.takeWhile(_ < guess)
        } yield goldbachTest(guess, prime.toInt)

        // is at least one test result true?
        val goldbachTestResult = goldbachTestResults.forall(!_)

        // stop processing if goldbach test fails
        !goldbachTestResult
      }
    })
    
    q.last
  }

  println(goldbachCounterExample())
}
