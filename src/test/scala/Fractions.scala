import org.scalatest.FunSuite

class Fractions extends FunSuite {
  val one = Fraction(1)
  val two = Fraction(2)

  test("one plus one is two") {
    assert(one + one == two)
  }
}
