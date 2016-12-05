import scala.io.Source

/**
  * Created by miketrupkin on 11/29/16.
  */
object Problem_079 extends App {
  import Numbers._

  val keylog: Set[Int] = {
    Source.fromInputStream(getClass.getResourceAsStream("p079_keylog.txt")).
      getLines().map(_.toInt).toSet
  }

  val keylogDigits: Set[List[Int]] = keylog.map(_.digits)

  val possibleDigits = keylogDigits.flatten.toList.sorted

  println(possibleDigits.sortWith( (x, y) => {
    val evidence = keylogDigits.find(entry => entry.contains(x) && entry.contains(y)).get
    evidence.indexOf(x) < evidence.indexOf(y)
  } ).fromDigits() )
}
