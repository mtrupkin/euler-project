object Problem_002 extends App {
  val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)

  def apply(n: Int): Int = {
    val fs = Stream.from(1).map(fibs(_)).takeWhile(_ < n)
    val even = fs.filter(_%2==0)
    even.sum
  }

  println(Problem_002(4000000))
}
