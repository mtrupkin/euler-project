import org.scalatest.FunSuite

class ProblemSuite extends FunSuite {
  test("one plus one is two")(assert(1 + 1 == 2))


  test("multiples example") {
     assert(SumMultiples(10) === 23)
  }

  test("prime factorization example") {
     assert(LargestPrimeFactor(13195) === 29)
  }

  test("Collatz example") {
     assert(CollatzSequence(13).length === 10)
  }

}
