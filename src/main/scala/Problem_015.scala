import scala.collection.mutable

object Problem_015 extends App {

  def solve: Long = paths(20, 20)

  private val Paths = mutable.Map[(Int, Int), Long]()

  private def paths(x: Int, y: Int): Long = {
    if (x == 0)      1
    else if (y == 0) 1
    else {
      if (!Paths.contains(x, y)) {
        // Number of paths is sum of paths followed to arrive here
        Paths += (x, y) -> (paths(x - 1, y) + paths(x, y - 1))
      }
      Paths(x, y)
    }
  }

  println(paths(20, 20))
}
