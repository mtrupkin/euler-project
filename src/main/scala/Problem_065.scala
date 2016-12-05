
object Problem_065 extends App {
  def doubledTerms: Stream[Long] = {
    def doubled(k: Int): Stream[Long] = Stream(1, 2*k, 1)

    Stream.from(1).map(doubled).flatten
  }

  def continuedFraction(a0: Int, ns: IndexedSeq[Long], terms: Int): Fraction = {
    val length = ns.length

    def term(i: Int): Fraction = {
      def n: Long = ns(i % length)
      (terms - i) match {
        case 1 => 0
        case 2 => Fraction(1, n)
        case _ => {
          val r = term(i+1)
          1 / (n + r)
        }
      }
    }

    a0 + term(0)
  }

  def e(terms: Int): Fraction = continuedFraction(2, doubledTerms.take(terms).toVector, terms)

  val _e = e(100)

  println(s"${_e}")
  println(s"Approx E: ${_e.value(10)}")
  println(s"  Math.E: ${Math.E}")
  val sum = Numbers.toDigits(_e.numerator).sum
  println(s"sum of numerator: $sum")


}
