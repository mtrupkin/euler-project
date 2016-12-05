/**
  * Created by miketrupkin on 12/5/16.
  */
object Problem_081 extends App {
  val rows = Numbers.getLines("p081_matrix.txt")
  val matrix = rows.map(_.split(",").map(_.toInt))

}
