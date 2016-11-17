import scala.collection.mutable

/**
  * Problem - 14
  *
  * n -> n / 2 (n is even)
  * n -> 3n + 1 (n is odd)
  *
  * Starting with 13
  * 13, 40, 20, 10, 5, 16, 8, 4, 2, 1
  */
object CollatzSequence extends App {
  lazy val cache_ : mutable.Map[Long, List[Long]] = mutable.Map.empty

  def next(n: Long): Long = {
    n match {
      case _ if n % 2 == 0 => n / 2
      case _ => 3 * n + 1
    }
  }

  def cache(x: Long, xs: List[Long]): Unit = {
    if (!cache_.contains(x)) {
      xs match {
        case n :: _ => {
          cache_(x) = xs
          cache(xs.head, xs.tail)
        }
        case _ =>
      }
    }
  }

  def apply(n: Long): List[Long] = {
    def collatz(n: Long, acc: List[Long]): List[Long] = {
      cache_.get(n) match {
        case Some(hit) => {
          val res = (acc :+ n) ++ hit
          cache(res.head, res.tail)
          res
        }
        case None => {
          if (n == 1) {
            val res = acc :+ n
            cache(res.head, res.tail)
            res
          } else {
            val n1 = next(n)
            collatz(n1, acc :+ n)
          }
        }
      }

    }

   collatz(n, List())
  }

  val sequences = (1 to 1000000).map(CollatzSequence(_))

  val longestSequence = sequences.maxBy(_.length)
  println(longestSequence.head)

}
