/**
  * Created by miketrupkin on 11/14/16.
  */
object CircularPrimes extends App {
  def circulars(n: Int): List[Int] = {
    def rotate(ns: List[Int], acc: List[Int]): List[Int] = {
      def toInt(ns: List[Int]): Int = {

        val q = ns.map(_.toString)
        q.mkString.toInt
      }

      if( acc.length < ns.length) {
        val rotated = ns match {
          case head :: tail => tail :+ head
          case head :: Nil => List(head)
        }

        rotate(rotated, toInt(rotated) :: acc)
      } else acc
    }

    val digits = Numbers.toDigits(n)
    val q = rotate(digits, Nil)
    q
  }

  def circularPrime(n: Int): Boolean = {
    val q = circulars(n)
    q.forall(Primes.isPrime(_))
  }


  def apply(n: Int): Int = (1 until n).filter(circularPrime(_)).length

  println(CircularPrimes(100))
  println(CircularPrimes(1000000))
}
