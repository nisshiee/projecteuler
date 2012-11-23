package projecteuler

import scalaz._, Scalaz._

object P55 extends App {

  def process = (n: BigInt) => n.toString.reverse |> BigInt.apply |> (n +)
  def isPalindrome = (n: BigInt) => n.toString == n.toString.reverse
  def checkStepK = Kleisli(process >>> { n => !isPalindrome(n) option n })
  def isLychrel = { (_: Option[_]).isDefined } <<< List.fill(50)(checkStepK).reduce(_ >=> _)

  (1 until 10000).map(BigInt.apply).filter(isLychrel).size |> println
}
