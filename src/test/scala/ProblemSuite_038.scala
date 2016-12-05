import org.scalatest.{FunSuite, Matchers}

class ProblemSuite_038 extends FunSuite with Matchers {
  import Problem_038._

  test("concatenated product example") {
    withClue("concatenated product of 192 and (1,2,3) equals 192384576") {
      val actual = concatenatedProduct(192, 3)
      actual should equal (192384576)
    }

    withClue("concatenated product of 9 and (1,2,3,4,5) equals 918273645") {
      val actual = concatenatedProduct(9, 5)
      actual should equal (918273645)
    }
  }
}
