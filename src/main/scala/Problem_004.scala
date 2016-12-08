/**
  * Find the largest palindrome made from the product of two 3-digit numbers.
  */
object Problem_004 extends App {
  import Numbers._

  // largest number as a product of two 3-digit numbers
  lazy val largestProduct = 998001 // 999 * 999

  def isPalindrome(n: Int): Boolean = {
    val digits = n.digits
    digits == digits.reverse
  }

  def isThreeDigits(n: Long): Boolean = n.digits.length == 3

  // is both numbers in tuple three digits
  def isThreeDigits(n: (Long, Long)): Boolean = isThreeDigits(n._1) && isThreeDigits(n._2)

  // list of all two factors of a number
  // where n = a * b
  def products(n: Int): List[Long] = {
    def products(a: Int, acc: List[Long]): List[Long] = {
      if ( a >= n ) acc else
      if ((n % a) == 0) products(a + 1, a :: acc)
      else products(a + 1, acc)
    }

    products(2, Nil)
  }

  def products2(n: Int): List[(Long, Long)] = {
    val factors = Primes.primeFactors(n)
    println(s"n: $n factors: $factors\n")
    val factorsLength = factors.length
    // optimization
    if (factorsLength == 2) return List((factors(0), factors(1)))
    val products = for {
      factorCount1 <- (1 to factorsLength/2).toList
      factorCount2 = factorsLength - factorCount1
    } yield {
      // all combinations of factors from 1 up to half the length of factors
      val q1 = factors.combinations(factorCount1).map(_.product).toList
      // all remaining combinations of factors
      val q2 = factors.combinations(factorCount2).map(_.product).toList.reverse
      println(q1)
      println(q2)
      println
      q1.zip(q2)
    }
    println
    println(products.flatten)
    println
    products.flatten
  }

  // find the next smallest palindrome
  def prevPalindrome(last: Int): Int = Iterator.from(last-1, -1).find(isPalindrome).get

  // find palindrome that is product of two 3-digit numbers
  def searchPalindrome(guess: Int): Int = {
    val palindrome = prevPalindrome(guess)
    val ps = products(palindrome).map(x => (x, palindrome/x))
    if (ps.exists(isThreeDigits)) palindrome
      else searchPalindrome(palindrome)
  }

  def apply(): Int = searchPalindrome(largestProduct)

  println(this())
}
