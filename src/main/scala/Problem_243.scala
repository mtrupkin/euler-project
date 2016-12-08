import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.concurrent.duration
import scala.concurrent.duration.Duration

/**
  * Created by miketrupkin on 12/6/16.
  */
object Problem_243 extends App {
  import Fraction._

  def memoize[I, O](f: I => O): collection.Map[I, O] = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  lazy val factors2: Int => Seq[Long] = memoize {
    Primes.primeFactors(_)
  }

  val factors: mutable.Map[Int, Set[Long]] = new mutable.HashMap[Int, Set[Long]]() {
    override def apply(key: Int) = getOrElseUpdate(key, Primes.primeFactors(key).toSet)
  }

  var time = System.nanoTime()

  def resilience(d: Int): Int = {
    if (d % 1000 == 0) {
      import scala.concurrent.duration._
      val now = System.nanoTime()
      val elapsed: Duration = (now - time).nanoseconds
      time = now
      println(s"$d elapsed: ${elapsed.toSeconds}")
    }

    val denominatorFactors = factors(d)
    println(denominatorFactors)
    val notResilientCount = (2 until d).count(factors(_).exists(denominatorFactors.contains(_)))
    val notResilient = (2 until d).filter(factors(_).exists(denominatorFactors.contains(_)))
    println(notResilient)
//    val count = (d - notResilient.length - 1)
    (d - notResilientCount - 1)
  }

  def resilienceRatio(d: Int): Double = resilience(d) / (d - 1)

  def printR(d: Int): Unit = println(s"R($d): ${resilience(d)}\n")

  printR(12)
  printR(4)
  printR(10)

  println(factors(3))


  val target: Double  = 15499 / 94744
//  val d = Stream.from(2).find(resilienceRatio(_) < target).get


//  println(s"d: $d")
//  println(s"d: ${resilience(d).value(5)}")
//  println(s"target: ${target.value(5)}")
}
