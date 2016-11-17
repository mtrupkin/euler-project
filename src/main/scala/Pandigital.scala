/**
  * Created by miketrupkin on 11/15/16.
  */
object Pandigital extends App {
  import Numbers.toDigits

  // returns true if product is pandigital
  def isPandigitalProduct(multiplicand: Int, multiplier: Int, product: Int): Boolean = {
    val multiplicandDigits = toDigits(multiplicand)
    val multiplierDigits = toDigits(multiplier)
    val productDigits = toDigits(product)

    val allDigits = multiplicandDigits ++ multiplierDigits ++ productDigits
    (allDigits.length == 9) && (allDigits.toSet.size == 9) && (!allDigits.contains(0))
  }

  // returns list of all parts of a pandigital number of with the specified number of digits
  // ie digits = 2 returns List(12, 13, 14, ...)
  // ie digits = 3 returns List(123, 124, 125, ...)
  def pandigitalParts(digits: Int): List[Int] = {
    def isPandigitPart(n: Int): Boolean = toDigits(n).toSet.size == digits

    val start = Math.pow(10, digits-1).toInt
    val finish = Math.pow(10, digits).toInt

    (start until finish).filter(isPandigitPart).toList
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
    // all padigital products in the form:
    // x * xxxx = xxxx
    val pandigitals1 = pandigitals(1, 4)

    // all padigital products in the form:
    // xx * xxx = xxxx
    val pandigitals2 = pandigitals(2, 3)

    val allPandigitals = pandigitals1 ++ pandigitals2

    val products = allPandigitals.map(_._3)

    // drop duplicate products and return sum
    products.toSet.sum
  }

  println(Pandigital())
}
