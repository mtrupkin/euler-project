import org.scalatest.{FunSuite, Matchers}

class ProblemSuite_089 extends FunSuite with Matchers {

  import Problem_089._

  test("roman numeral to arabic sixteen") {
    val sixteen = List("IIIIIIIIIIIIIIII", "VIIIIIIIIIII", "VVIIIIII", "XIIIIII", "VVVI", "XVI")

    sixteen.map(parseRoman).foreach(_ should equal(16))
  }

  test("roman numeral 'D' to arabic") {
    val roman = "D"

    parseRoman(roman) should equal(500)
  }


  test("year 1606 to roman") {
    toRoman(1606) should equal ("MDCVI")
  }
}
