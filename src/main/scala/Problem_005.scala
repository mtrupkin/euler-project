/**
  * Smallest positive number that is evenly divisible by all of the numbers from 1 to 20
  *
  * 9699690 is the product of all primes less than 20
  */
object Problem_005 extends App {
  def isMultiple(n: Long): Boolean = (2 to 20).forall(n % _ == 0)

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
