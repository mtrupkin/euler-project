/**
  * Created by miketrupkin on 11/15/16.
  */
object Pandigital {
  import Numbers.DigitExtractor

  implicit class PandigitalDetector[B](n: B) {
    def isPandigital(implicit num: Integral[B]): Boolean = {
      val digits = n.digits
      (digits.toSet.size == 9) && (!digits.contains(0))
    }
  }

  // find the next smallest pandigital
  def prevPandigital(last: Int): Int = Iterator.from(last-1, -1).find(_.isPandigital).get
}
