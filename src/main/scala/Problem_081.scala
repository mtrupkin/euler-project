/**
  * Created by miketrupkin on 12/5/16.
  */
object Problem_081 extends App {
  val rows = Numbers.getLines("p081_sample-matrix.txt")
  val matrix = rows.map(_.split(",").map(_.toInt)).toSeq

  val size = 5

  val weightMap = new DijkstraMap {
    def apply(p: Point): Int = matrix(p.y)(p.x)

    def canMove2(p1: Point, p0: Point): Boolean = true

    def canMove(p1: Point, p0: Point): Boolean = {
      val p = p1 - p0
      val noDiagonal =  ((p.x == 0) || (p.y == 0)) // no diagonal
      val rightOrDown =  ((p.x == 1) || (p.y == 1))
      noDiagonal && rightOrDown
    }
  }

  val p0 = Point(0, 0)
  val dijkstra = new Dijkstra(size, weightMap)
  dijkstra.search(p0)

  val path = p0 +: dijkstra.path(Point(size-1, size-1))

  println(weightMap(Point(1, 0)))

  println(path)
  println(path.map(weightMap(_)))
  println(path.map(dijkstra.nodes(_).dist))
  println(path.map(weightMap(_)).sum)

  (0 until size).foreach(y => {
    println
    (0 until size).foreach(x => {
      if (path.contains(Point(x,y))) print("*") else print(".")
    })
  })
  println
}
