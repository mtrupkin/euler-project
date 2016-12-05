/**
  * Created by miketrupkin on 11/25/16.
  */
object Problem_038 extends App {
  import Numbers._
  import Pandigital._

  val largestPandigital = 987654321
  val smallestPandigital = 918273645


  // multiply p by each of 1, 2, .. n and concatenate each product
  // ie. 192384576 is the concatenated product of 192 and (1,2,3)
  def concatenatedProduct(p: Int, n: Int): BigInt = {
    val q = (1 to n).flatMap(x => (x * p).digits).fromDigits

    // println(s"$p $n $q")
    q
  }

  val guess = concatenatedProduct(987, 2)
  guess.isPandigital

  def search(guess: Int): Int = {
    println(s"guess: $guess")

    val guessDigitSize = 4
    val guessMaxN = 9
    val product = guess.digits.take(guessDigitSize)

    val res = for {
      i <- (1 to guessDigitSize).toList
      p = product.take(i).fromDigits().toInt
      n <- (2 to guessMaxN)
    } yield concatenatedProduct(p, n)

    res.find(_ == guess) match {
      case Some(pandigital) => pandigital.toInt
      case None => search(Pandigital.prevPandigital(guess))
    }
  }


  println(search(largestPandigital))
}
