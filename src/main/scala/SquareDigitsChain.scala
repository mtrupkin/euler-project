import scala.collection.mutable

/**
  * Created by miketrupkin on 11/14/16.
  */
object SquareDigitsChain extends App {

  lazy val f_ : mutable.Map[Int, Int] = mutable.Map.empty

  def nextChain(n: Int): Int = {
    val digits = n.toString.map( _.asDigit )
    val squares = digits.map(x => x*x)
    val sum = squares.sum
//    println(sum)
    sum
  }


  def chain(start: Int, n: Int): Int = {
    f_.getOrElse(start,
      if ((n == 89) || (n == 1)) {
        f_(start) = n
        n
      } else chain(start, nextChain(n))
    )
  }

  println(chain(44, nextChain(44)))
  println(chain(85, nextChain(85)))
  println(chain(1, nextChain(1)))

  println(Iterator.from(1).takeWhile( _ < 10000000).map( x => {
    chain(x, nextChain(x))}).filter( _ == 89).length)
}
