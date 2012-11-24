package projecteuler.math

import scalaz._, Scalaz._, Tags._

class Fraction(n: BigInt, d: BigInt)
extends Tuple2[BigInt, BigInt](Fraction._numerator(n, d), Fraction._denominator(n, d)) {

  def n = _1
  def d = _2

  override def toString = _1.toString |+| " / " |+| _2.toString
}

object Fraction {

  private def _numerator(n: BigInt, d: BigInt): BigInt = (n === 0, d === 0) match {
    case (true, _) => 0
    case (_, true) => 1
    case _ => n.abs / gcd(n, d) * ((n < 0 ^ d < 0)? -1 | 1)
  }

  private def _denominator(n: BigInt, d: BigInt): BigInt = (d =/= 0)? (d.abs / gcd(n, d)) | 1

  def apply(n: BigInt, d: BigInt) = new Fraction(n, d)

  implicit def toOps(x: Fraction): FractionOps = new FractionOps { val self = x }
  implicit def toFraction[A <% BigInt](x: A) = Fraction(x, 1)
}

trait FractionInstances {

  implicit val fractionGroup = new Group[Fraction] {

    def append(f1: Fraction, f2: => Fraction): Fraction = Fraction(f1.n * f2.d + f2.n * f1.d, f1.d * f2.d)

    def inverse(f: Fraction): Fraction = Fraction(-f.n, f.d)

    val zero: Fraction = Fraction(0, 1)
  }

  implicit val fractionMultiplicationSemiGroup = new Semigroup[Fraction @@ Multiplication] {

    type F = Fraction @@ Multiplication
    def F(n: BigInt, d: BigInt) = Multiplication(Fraction(n, d))

    def append(f1: F , f2: => F): F = F(f1.n * f2.n, f1.d * f2.d)
    val zero: F = F(1, 1)
  }

  implicit val fractionShow = Show.showA[Fraction]
  implicit val fractionEqual = Equal.equalA[Fraction]
}

trait FractionOps {

  def self: Fraction

  def unary_~ = Fraction(self.d, self.n)
}

