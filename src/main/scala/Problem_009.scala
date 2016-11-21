/**
  * Pythagorean Triples
  */
object Problem_009 extends App {
  type Triple = (Int, Int, Int)
  def generatePythagoreanTriple(m: Int, n: Int): Triple = {
    val a = n*n - m*m
    val b = 2*n*m
    val c = n*n + m*m
    (a, b, c)
  }

  def sum(t: Triple): Int = t._1 + t._2 + t._3
  def product(t: Triple): Int = t._1 * t._2 * t._3

  def search(): Triple = {
    val triples = for {
      m <- 1 to 100
      n <- m to 100
    } yield generatePythagoreanTriple(m, n)

    triples.map(t => (t, sum(t))).find( _._2 == 1000).get._1
  }

  val triple = search()
  println(triple)
  println(product(triple))
}
