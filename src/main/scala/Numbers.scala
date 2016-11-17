/**
  * Created by miketrupkin on 11/15/16.
  */
object Numbers {
  def toDigits(n: Int): List[Int] = n.toString.map(_.asDigit).toList
}
