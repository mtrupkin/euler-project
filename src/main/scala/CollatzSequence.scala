/**
  * Problem - 14
  *
  * n -> n / 2 (n is even)
  * n -> 3n + 1 (n is odd)
  *
  * Starting with 13
  * 13, 40, 20, 10, 5, 16, 8, 4, 2, 1
  */
object CollatzSequence extends App {
  def next(n: Int): Int = {
    n match {
      case _ if n % 2 == 0 => n / 2
      case _ => 3 * n + 1
    }
  }

  def apply(n: Int): List[Int] = {
    def collatz(n: Int, acc: List[Int]): List[Int] = {
      val n1 = next(n)
      if (n1 == 1) {
        n1 :: acc
      } else {
        collatz(n1, n1 :: acc)
      }
    }

    collatz(n, List(n))
  }

  val sequences = (1 to 1000000).map(CollatzSequence(_))

  val longestSequence = sequences.maxBy(_.length)
  println(longestSequence.last)

}
