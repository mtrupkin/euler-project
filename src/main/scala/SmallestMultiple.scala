/**
  * Created by miketrupkin on 11/18/16.
  */
object SmallestMultiple extends App {
  // 9699690 product of all primes less than 20
  def isMultiple(n: Long): Boolean = {
    (2 to 20).forall(n % _ == 0)
  }

  def missingMultiple(n: Long): Long = {
    (2 to 20).find(n % _ != 0).get
  }

  def missingMultiples(n: Long): List[Long] = {
    (2 to 20).filter(n % _ != 0).map(_.toLong).toList
  }

  val ps = Numbers.primes_.takeWhile(_ < 20)
  println(ps)
  val n = ps.product
  println(n)
//  var n = 998001L
//  while (!isMultiple(n)) {
//    n = n * missingMultiple(n)
//  }

  println(missingMultiples(n*2*3*2*2))
  println(n*2*3*2*2)
}
