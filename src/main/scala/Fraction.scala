/**
  * Created by miketrupkin on 11/21/16.
  */

object Fraction {
  def apply(i: Int): Fraction = new Fraction(i, 1)
  def apply(i: Long): Fraction = new Fraction(i, 1)

  implicit def toFraction(i: Int) = Fraction(i)
  implicit def toFraction(i: Long) = Fraction(i)
}

case class Fraction(a: BigInt, b: BigInt) {
  require(b != 0)

  private lazy val gcd: BigInt = a.gcd(b)

  lazy val numerator: BigInt = a / gcd
  lazy val denominator: BigInt = b / gcd


  def + (f: Fraction): Fraction = Fraction(numerator * f.denominator + f.numerator * denominator, denominator * f.denominator)

  def * (f: Fraction): Fraction = Fraction(numerator * f.numerator, denominator * f.denominator)

  def / (f: Fraction): Fraction = Fraction(numerator * f.denominator, f.numerator * denominator)

  def value(scale: Int): BigDecimal = BigDecimal(numerator, scale) / BigDecimal(denominator, scale)

  def < (f: Fraction): Boolean = {
    val n1 = a * f.b
    val n2 = f.a * b
    n1 < n2
  }

  override def toString = s"$numerator / $denominator"
}