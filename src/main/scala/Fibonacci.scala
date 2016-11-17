import scala.collection.mutable

/**
  *
  */
object Fibonacci extends App {
  lazy val f_ : mutable.Map[Int, Int] = mutable.Map.empty

  def f(n: Int): Int = {
    f_.getOrElse(n, {
      n match {
        case 1 => 1
        case 2 => 2
        case _ => {
          val res = f(n-1) + f(n-2)
          f_(n) = res
          res
        }
      }})
  }

  def apply(n: Int): Int = {
    val fs = Stream.from(1).map(f(_)).takeWhile(_ < n)
    val even = fs.filter(_%2==0)
    even.sum
  }

  println(Fibonacci(4000000))
}
