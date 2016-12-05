/**
  * Find the difference between the sum of the squares
  * of the first one hundred natural numbers
  * and the square of the sum.
  */
object Problem_006 extends App {
  // first 100 natural numbers
  val n = (1 to 100)

  // sum of the squares
  val sumOfSquares = n.map(n => n * n).sum
  println(s"sum of squares: $sumOfSquares")

  // square of the sum
  val sum = n.sum
  val squareOfSums = sum * sum
  println(s"square of sums: $squareOfSums")

  // difference between the sum of the squares and the square of the sum
  println(s"difference: ${squareOfSums - sumOfSquares}")
}
