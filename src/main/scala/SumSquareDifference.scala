/**
  * Created by miketrupkin on 11/18/16.
  */
object SumSquareDifference extends App {
  val s1 = (1 to 100).map(n => n * n).sum
  println(s1)
  val s = (1 to 100).sum
  val s2 = s * s
  println(s2)
  println(s2 - s1)
}
