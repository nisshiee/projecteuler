package projecteuler

import scalaz._, Scalaz._
import projecteuler.math._

object P57 extends App {

  def roots = Stream.iterate(Fraction(1, 2)) { x: Fraction =>  ~(x |+| 2) } map (_ |+| 1)
  def isFindings = (x: Fraction) => x.n.toString.length > x.d.toString.length
 
  roots.take(1000).filter(isFindings).size |> println
}
