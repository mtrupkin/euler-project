/**  Problem - 1
  *  Sum the numbers that are multiples of 3 and 5
  *  from the list of numbers below n
  */
object SumMultiples extends App {
  def f(n: Int): Int = {
//    val multiples = (1 until n).filter(x => (x % 3 == 0) || (x % 5 == 0))
//    multiples.sum

    (1 until n)
      .filter(x => (x % 3 == 0) || (x % 5 == 0))
      .sum
  }
  println(SumMultiples.f(1000))
}

