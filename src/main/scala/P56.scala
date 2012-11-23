package projecteuler

import scalaz._, Scalaz._

object P56 extends App {

  val sumList = for {
    a <- 1 until 100
    b <- 1 until 100
  } yield BigInt(a).pow(b) |> { _.toString.map(_.toString.parseInt | 0) sum }
  val answer = sumList max

  println(answer)
}
