import scala.collection.mutable

/**
  * Created by miketrupkin on 12/6/16.
  */
object Problem_243 extends App {
  def memoize[I, O](f: I => O): collection.Map[I, O] = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  val notResilient: collection.Map[(Set[Int], Set[Int]), Boolean] = new mutable.HashMap[(Set[Int], Set[Int]), Boolean]() {
    def f(key: (Set[Int], Set[Int])): Boolean = {
      val (a, b) = key
      if (a.size < b.size) a.exists(b.contains(_)) else b.exists(a.contains(_))
    }

    override def apply(key: (Set[Int], Set[Int])) = {
      get(key) match {
        case Some(v) => v
        case None => val d = f(key); {
          this(key) = d
          this(key.swap) = d
        }; d
      }
    }
  }

  val factors: Seq[Set[Int]] = Stream.from(2).map(Primes.primeFactorSet(_))

  var time = System.nanoTime()

  var benchCounts = new mutable.HashMap[String, Int]
  import scala.concurrent.duration._
  def bench[R](s: String, freq: Int = 1000)(f: => R): R = {
    val count = benchCounts.getOrElse(s, 0)
    benchCounts(s) = count + 1
    if (count % freq == 0 ) {
      val t0 = System.nanoTime()
      val result = f
      // call-by-name
      val t1 = System.nanoTime()
      val elapsed: Duration = (t1 - t0).nanoseconds
      println(s"elapsed($s): ${elapsed.toMillis} ms")
      result
    } else f
  }

  def resilience(d: Int): Int = {
    bench(s"resilience", 1000) {
      val denominatorFactors =        factors.take(d).last

      //    println(s"$d $denominatorFactors")
      //    val notResilientCount = (2 until d).count(factors(_).exists(denominatorFactors.contains(_)))
      val notResilientCount =
        factors.take(d - 1).count(x => notResilient((x, denominatorFactors)))

      //    val notResilient = (2 until d).filter(factors(_).exists(denominatorFactors.contains(_)))
      //    println(notResilient)
      //    val count = (d - notResilient.length - 1)
      (d - notResilientCount - 1)
    }
  }

  def resilienceRatio(d: Int): Double = resilience(d) / (d - 1.0)

  def printR(d: Int): Unit = { bench("printR") { println(s"R($d): ${resilience(d) / (d - 1.0)}") }; println }

  printR(12)
  printR(4)
  printR(10)

  println(factors.take(3))


//  val target: Double  = 15499 / 94744.0
  val target: Double  = 18000 / 94744.0
  def d = Stream.from(2).find(resilienceRatio(_) < target).get

//  (10000 until 20000).foreach(printR)

  println(factors.take(5).toList)

  println(Primes.primeFactorSet(2000002))

  println(factors.take(5))
  println(s"d: $d")
//  println(s"d: ${resilience(d).value(5)}")
//  println(s"target: ${target.value(5)}")
}
