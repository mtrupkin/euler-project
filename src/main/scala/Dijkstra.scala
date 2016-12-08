
import scala.collection.mutable

case class Point(x: Int, y: Int) {
  def +(p: Point): Point = Point(x + p.x, y + p.y)
  def -(p: Point): Point = Point(x - p.x, y - p.y)
}

trait DijkstraMap {
  def apply(p: Point): Int
  def canMove(p: Point, p0: Point): Boolean
}

class Dijkstra(val dim: Int, val map: DijkstraMap) {

  protected case class Node(p: Point, dist: Double = Double.MaxValue) extends Ordered[Node] {
    override def compare(o: Node): Int = (o.dist-dist).toInt
    override def toString: String = s"$p dist: $dist"
    def weight: Int = map(p)
  }

  val nodes = new mutable.HashMap[Point, Node]()
  protected var start: Option[Point] = None

  // dijkstra's algorithm using a binary heap.
  // find all paths starting with p0
  def search(p0: Point): Unit = {
    var q = new mutable.PriorityQueue[Node]()
    start = Some(p0)

    foreach(p0, dim, p => nodes(p) = Node(p))

    // add source node
    val node0 = nodes(p0)
    val source = node0.copy(dist = node0.weight)
    nodes(p0) = source
    q += source

    while (!q.isEmpty) {
      // node with shortest distance
      val u = q.dequeue()

      // look at each neighbour
      for {
        v <- nodeNeighbors(u.p)
        newDist = u.dist + v.weight

        if map.canMove(v.p, u.p)
        if newDist <= v.dist
      } {
        // shorter path to neighbour found
        val newNode = v.copy(dist = newDist)
        nodes(v.p) = newNode
        q += newNode
      }
    }
  }

  protected def nodeNeighbors(p: Point): Seq[Node] = {
    for {
      n <- neighbors(p)
    } yield nodes(n)
  }

  def moveCount(p: Point, p0: Point, r: Int): Int = {
    if (!start.isDefined) {
      search(p0)
    }
    nodes(p).dist.toInt
  }

  def path(p: Point): Seq[Point] = {
    def path(p: Point, acc: List[Point]): Seq[Point] = {
      if (p == start.get) return acc

      val ns = for {
        n <- neighbors(p)
        if nodes(n).dist < nodes(p).dist
        if map.canMove(p, n)
      } yield n

      if (ns != Nil) path(ns.minBy(nodes(_).dist), p :: acc) else Nil
    }

    if (nodes.contains(p)) path(p, Nil) else Nil
  }

  implicit def TupleToPoint(t: (Int, Int)): Point = Point(t._1, t._2)
  implicit def PointToTuple(p: Point): (Int, Int) = (p.x, p.y)


  def foreach(p: Point, r: Int, f: (Point => Unit)): Unit = {
    for {
      x <- 0 until r
      y <- 0 until r
    } f(p + (x, y))
  }

  def neighbors(p: Point, r: Int = 1): Seq[Point] = {
    for {
      x <- -r to r
      y <- -r to r
      if !((x == 0) && (y == 0))
      p0 = p + (x, y)
      if nodes.contains(p0)
    } yield p0
  }
}
