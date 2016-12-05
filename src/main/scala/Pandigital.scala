/**
  * Created by miketrupkin on 11/15/16.
  */
object Pandigital extends App {
  import Numbers.DigitExtractor

  implicit class PandigitalDetector[B](n: B) {
    def isPandigital(implicit num: Integral[B]): Boolean = {
      val digits = n.digits
      (digits.toSet.size == 9) && (!digits.contains(0))
    }
  }

  // find the next smallest pandigital
  def prevPandigital(last: Int): Int = Iterator.from(last-1, -1).find(_.isPandigital).get

  // returns true if product is pandigital
  def isPandigitalProduct(multiplicand: Int, multiplier: Int, product: Int): Boolean = {
    val multiplicandDigits = multiplicand.digits
    val multiplierDigits = multiplier.digits
    val productDigits = product.digits

    val allDigits = multiplicandDigits ++ multiplierDigits ++ productDigits
    (allDigits.toSet.size == 9) && (!allDigits.contains(0))
  }

  // returns list of all parts of a pandigital number of with the specified number of digits
  // ie digits = 2 returns List(12, 13, 14, ...)
  // ie digits = 3 returns List(123, 124, 125, ...)
  def pandigitalParts(digits: Int): List[Int] = {
    def isPandigitalPart(n: Int): Boolean = n.digits.toSet.size == digits

    val start = Math.pow(10, digits-1).toInt
    val finish = Math.pow(10, digits).toInt

    (start until finish).filter(isPandigitalPart).toList
  }

  // returns all pandigital products with the given multiplicand and multiplier size
  def pandigitals(multiplicandSize: Int, multiplierSize: Int): List[(Int, Int, Int)] = {
    val multiplicands = pandigitalParts(multiplicandSize)
    val multipliers = pandigitalParts(multiplierSize)

    val identities = for {
      multiplicand <- multiplicands
      multiplier <- multipliers
    } yield (multiplicand, multiplier, multiplicand * multiplier)

    identities.filter((isPandigitalProduct _).tupled)
  }


  def apply(): Int = {
    // all pandigital products in the form:
    // x * xxxx = xxxx
    val pandigitals1 = pandigitals(1, 4)

    // all pandigital products in the form:
    // xx * xxx = xxxx
    val pandigitals2 = pandigitals(2, 3)

    val allPandigitals = pandigitals1 ++ pandigitals2

    val products = allPandigitals.map(_._3)

    // drop duplicate products and return sum
    products.toSet.sum
  }

  println(Pandigital())
}
