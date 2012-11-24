package projecteuler.math

import scalaz._, Scalaz._

trait NumberTheory {

  def gcd(x: BigInt, y: BigInt): BigInt = {
    if (x === 0 & y === 0) 1
    else {
      def rec(l: BigInt, s: BigInt): BigInt = l % s match {
        case n if n == BigInt(0) => s
        case n => rec(s, n)
      }
      val xabs: BigInt = (x =/= 0)? x.abs | y.abs
      val yabs: BigInt = (y =/= 0)? y.abs | x.abs
      (xabs > yabs)? rec(xabs, yabs) | rec(yabs, xabs)
    }
  }
}
