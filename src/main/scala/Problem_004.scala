/**
  * Created by miketrupkin on 11/17/16.
  */
object Problem_004 extends App {
  def isPalindrome(n: Int): Boolean = {
    val digits = Numbers.toDigits(n)
    digits == digits.reverse
  }

  def products(n: Int): List[(Long, Long)] = {
    val factors = Numbers.primeFactors(n)
    val factorsLength = factors.length
    val products = for {
      factorCount1 <- (1 to factorsLength/2).toList
      factorCount2 = factorsLength - factorCount1
    } yield {
      val q1 = factors.combinations(factorCount1).map(_.product)
      val q2 = factors.combinations(factorCount2).map(_.product)
      q1.zip(q2.toList.reverse.toIterator)
    }
    val l = products.flatten
    l
  }

  def nextPalindrome(last: Int): Int = {
    Iterator.from(last-1, -1).takeWhile(_ > 10000).find(isPalindrome).get
  }

  def threeDigits(n: Long): Boolean = Numbers.toDigits(n.toInt).length == 3

  var palindrome = 998001
  while (true) {
    palindrome = nextPalindrome(palindrome)
    val ps = products(palindrome)
    ps.find(x => threeDigits(x._1) && threeDigits(x._2)).
      map(y => println(s"$palindrome: $y"))

  }

}
