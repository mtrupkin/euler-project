/**
  * Created by miketrupkin on 11/15/16.
  */
object Pandigital extends App {
  import Numbers.toDigits

  def isPandigital(multiplicand: Int, multiplier: Int, product: Int): Boolean = {
    val multiplicandDigits = toDigits(multiplicand)
    val multiplierDigits = toDigits(multiplier)
    val productDigits = toDigits(product)

    val allDigits = multiplicandDigits ++ multiplierDigits ++ productDigits
    (allDigits.length == 9) && (allDigits.toSet.size == 9) && (!allDigits.contains(0))
  }

  def pandigitalParts(digits: Int): List[Int] = {
    def isPandigitPart(n: Int): Boolean = toDigits(n).toSet.size == digits

    val start = Math.pow(10, digits-1).toInt
    val finish = Math.pow(10, digits).toInt

    (start until finish).filter(isPandigitPart).toList
  }

  def pandigitals(multiplicandSize: Int, multiplierSize: Int): List[(Int, Int, Int)] = {
    val multiplicands = pandigitalParts(multiplicandSize)
    val multipliers = pandigitalParts(multiplierSize)

    val identities = for {
      multiplicand <- multiplicands
      multiplier <- multipliers
    } yield (multiplicand, multiplier, multiplicand * multiplier)

    identities.filter((isPandigital _).tupled)
  }


  def apply(): Int = {
    val pandigitals1 = pandigitals(1, 4)
    val pandigitals2 = pandigitals(2, 3)

    println(pandigitals1)
    println(pandigitals2)

    val allPandigitals = pandigitals1 ++ pandigitals2

    val products = allPandigitals.map(_._3)
    println(products.length)
    println(products.toSet.size)
    products.toSet.sum
  }

  println(Pandigital())
}
