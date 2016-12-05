import org.scalatest.{FunSuite, Matchers}

class ProblemSuite_027 extends FunSuite with Matchers {

  import Problem_027._

  test("n^2 + n + 41") {
    consecutivePrimes(quadratic(1, 41)) should equal (40)
  }

  test("n^2 - 79n + 1601") {
    consecutivePrimes(quadratic(-79, 1601)) should equal (80)
  }
}
