import scala.collection.immutable.HashMap

/**
  * Created by miketrupkin on 11/29/16.
  *
  * Roman numerals
  */
object Problem_089 extends App {
  lazy val romanToArabic = HashMap(
    'I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

  lazy val arabicToRoman = List(
    1000 -> "M", 900 -> "CM", 500 -> "D", 400 -> "CD",
    100 -> "C", 90 -> "XC", 50 -> "L", 40 -> "XL",
    10 -> "X", 9 -> "IX", 5 -> "V", 4 -> "IV",
    1 -> "I")

  def parseRoman(s: String): Int = {
    require(s.length > 0)

    if (s.length == 1) romanToArabic(s.charAt(0)) else {
      // map list of chars to list of ints
      val numerals = s.toList.map(romanToArabic)
      val total = numerals.sliding(2).foldLeft(0)((a, b) => {
        val num = b(0)
        val nextNum = b(1)
        if (num < nextNum) a - num else a + num
      })

      total + numerals.last
    }
  }

  def toRoman(n: Int): String = {
    def toRoman(n: Int, acc: List[Char]): List[Char] = {
      arabicToRoman.find(n - _._1 >= 0) match {
        case Some((arabic, roman)) => toRoman(n - arabic, acc ++ roman)
        case None => acc
      }
    }

    toRoman(n, List.empty).mkString
  }

  val romanNumerals: Iterator[String] = Numbers.getLines("p089_roman.txt")

  val reduced = romanNumerals.map(roman => {
    val arabic = parseRoman(roman)
    val optimized = toRoman(arabic)
    roman.length - optimized.length
  }).sum

  println(reduced)
}
