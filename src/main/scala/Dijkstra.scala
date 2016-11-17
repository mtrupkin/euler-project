
import scala.collection.mutable

case class Point(x: Int, y: Int) {
  def +(p: Point): Point = Point(x + p.x, y + p.y)
  def -(p: Point): Point = Point(x - p.x, y - p.y)
}

/**
 * Created by mtrupkin on 1/2/2015.
 */
class Dijkstra(val dim: Int) {

  protected case class Node(p: Point, weight: Int = 1, dist: Double = Double.MaxValue, count: Int = 0) extends Ordered[Node] {
    override def compare(o: Node): Int = (o.dist-dist).toInt
    override def toString: String = s"$p dist: $dist count: $count"
  }

  protected val nodes = new mutable.HashMap[Point, Node]()
  protected var start: Option[Point] = None

  // dijkstra's algorithm using a binary heap.
  // find all paths starting with p0
  def search(p0: Point): Unit = {
    var q = new mutable.PriorityQueue[Node]()
    start = Some(p0)

    foreach(p0, dim, p => nodes(p) = Node(p))


    // add source node
    val source = nodes(p0).copy(dist = 0)
    nodes(p0) = source
    q += source

    while (!q.isEmpty) {
      // node with shortest distance
      val u = q.dequeue()

      // look at each neighbour
      for {
        v <- nodeNeighbors(u.p)
        newDist = u.dist + v.weight

        if newDist <= v.dist
      } {
        // shorter path to neighbour found
        val newNode = v.copy(dist = newDist, count = u.count + 1)
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

  protected def path(p: Point, acc: List[Point]): Seq[Point] = {
    val dist = nodes(p).dist

    if (dist == 0) return acc

    val ns = for {
      n <- neighbors(p)
      if nodes(n).dist < dist
    } yield n

    if (ns != Nil) path(ns.head, p :: acc) else Nil
  }

  def moveCount(p: Point, p0: Point, r: Int): Int = {
    if (!start.isDefined) {
      search(p0)
    }
    nodes(p).dist.toInt
  }

  def path(p: Point, p0: Point): Seq[Point] = if (nodes.contains(p)) path(p, Nil) else Nil

  def pathsCount(p: Point, p0: Point): Int = {
    if (start.isEmpty) {
      search(p0)
    }
    val ps = p0 +: path(p, p0)
    val ns = ps.map(p => {
      val q = nodes(p)
      q
    })
    ns.map(_.count).sum
  }

  implicit def TupleToPoint(t: (Int, Int)): Point = Point(t._1, t._2)
  implicit def PointToTuple(p: Point): (Int, Int) = (p.x, p.y)


  def foreach(p: Point, r: Int, f: (Point => Unit)): Unit = {
    for {
      x <- 0 to r
      y <- 0 to r
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
