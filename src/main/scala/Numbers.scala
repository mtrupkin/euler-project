import scala.io.Source

/**
  * Created by miketrupkin on 11/15/16.
  */
object Numbers {
  implicit class DigitExtractor[B](n: B) {
    def digits(implicit num: Integral[B]): List[Int] = n.toString.map(_.asDigit).toList
  }

  implicit def seqWithDigits(t: Seq[Int])(implicit m: Numeric[Int]) = new {
    def fromDigits(): BigInt = BigInt(t.mkString)
  }

  def toDigits(n: BigInt): List[Int] = n.toString.map(_.asDigit).toList

  lazy val pascal = Stream.iterate(Seq(1L))(a=>(0L+:a,a:+0L).zipped.map(_+_))

  def binomialCoefficient(n: Int, k: Int) =
    (BigInt(n - k + 1) to n).product / (BigInt(1) to k).product

  def getLines(file: String): Iterator[String] =
    Source.fromInputStream(getClass.getResourceAsStream(file)).getLines()

}
