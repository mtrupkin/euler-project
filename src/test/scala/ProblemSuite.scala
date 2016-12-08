import org.scalatest.{FunSuite, Matchers}

class ProblemSuite extends FunSuite with Matchers {
  test("one plus one is two")(assert(1 + 1 == 2))


  test("multiples example") {
     assert(Problem_001(10) === 23)
  }

  test("Collatz example") {
     assert(CollatzSequence(13).length === 10)
  }

  test("Count paths example") {
    assert(Problem_015.paths(2, 2) === 6)
  }

  test("Problem 4 solution") {
    assert(Problem_004() === 906609)
  }


  test("Pandigital example") {
    assert(Problem_032.isPandigitalProduct(39, 186, 7254) === true)
  }

  test("Pandigital duplicate twos") {
    assert(Problem_032.isPandigitalProduct(2, 6729, 13458) === false)
  }

  test("Prime factors") {
    assert(Primes.primeFactors(13195).contains(29) === true)
  }

  test("continued fractions square root of two example") {
    def squareRoot(depth: Int): Fraction = Problem_065.continuedFraction(1, Vector(2), depth)

    withClue("term 1") {
      squareRoot(1)  should equal (Fraction(1))
    }

    withClue("term 2") {
      squareRoot(2)  should equal (Fraction(3, 2))
    }

    withClue("term 3") {
      squareRoot(3)  should equal (Fraction(7, 5))
    }

    withClue("term 4") {
      squareRoot(4)  should equal (Fraction(17, 12))
    }

    withClue("term 10") {
      squareRoot(10)  should equal (Fraction(3363, 2378))
    }
  }

  test("continued fractions square root of twenty three example") {
    val tolerance = 0.00001
    val approximation: Double = Problem_065.continuedFraction(4, Vector(1, 3, 1, 8), 8).value(4).toDouble

    import Math._
    abs(sqrt(23) - approximation) should be < tolerance
  }
}
