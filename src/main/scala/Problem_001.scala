/**  Problem - 1
  *  Sum the numbers that are multiples of 3 and 5
  *  from the list of numbers below n
  */
object Problem_001 extends App {
  def apply(n: Int): Int = (1 until n).filter(x => (x % 3 == 0) || (x % 5 == 0)).sum

  println(Problem_001(1000))
}

