import org.scalatest.{FunSuite, Matchers}

/**
  * Created by miketrupkin on 12/8/16.
  */
class PrimesSuite extends FunSuite with Matchers {
  test("prime factorization example") {
    assert(Primes.primeFactors(13195).head === 29)
  }

  test("prime factorization problem 3") {
    assert(Primes.primeFactors(600851475143L).head === 6857)
  }

  test("prime factorization of primes") {
    def primeTest(prime: Long): Boolean = Primes.primeFactors(prime).head === prime

    withClue ("3 is prime") {
      assert(primeTest(3))
    }

    withClue ("6857 is prime") {
      assert(primeTest(6857))
    }
  }

  test("Prime factors of 13195") {
    assert(Primes.primeFactors(13195).contains(29) === true)
  }
}
