package projecteuler

import scalaz._, Scalaz._
import projecteuler.math._

object P58 extends App {

  case class Step(
     tr: Int
    ,tl: Int
    ,bl: Int
    ,br: Int
    ,inc: Int
    ,dig: Int
    ,prime: Int
    ,len: Int
    ,ratio: Double
  )

  def firstStep = Step(1, 1, 1, 1, 0, 1, 0, 1, 0.0)
  def steps = Stream.iterate(firstStep) {
    case Step(tr, tl, bl, br, inc, dig, prime, len, _) => {
      val (ntr, ntl, nbl, nbr, ninc, ndig, nlen) = (
         tr + inc + 2
        ,tl + inc + 4
        ,bl + inc + 6
        ,br + inc + 8
        ,inc + 8
        ,dig + 4
        ,len + 2
      )
      val nprime = prime + List(ntr, ntl, nbl, nbr).map(BigInt.apply).filter(isPrime).size
      val nratio = nprime.toDouble / ndig
      Step(ntr, ntl, nbl, nbr, ninc, ndig, nprime, nlen, nratio)
    }
  }

  steps drop 1 find (_.ratio < 0.1) foreach (_.len |> println)
}
