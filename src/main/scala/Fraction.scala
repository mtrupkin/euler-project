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

  private val gcd: BigInt = a.gcd(b)

  val numerator: BigInt = a / gcd
  val denominator: BigInt = b / gcd

  def + (f: Fraction): Fraction = Fraction(numerator * f.denominator + f.numerator * denominator, denominator * f.denominator)

  def * (f: Fraction): Fraction = Fraction(numerator * f.numerator, denominator * f.denominator)

  def / (f: Fraction): Fraction = Fraction(numerator * f.denominator, f.numerator * denominator)

  def value(scale: Int): BigDecimal = BigDecimal(numerator, scale) / BigDecimal(denominator, scale)

  override def toString = s"$numerator / $denominator"
}