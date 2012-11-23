package projecteuler

import scalaz._, Scalaz._
import scalax.io._
import projecteuler.card._
import projecteuler.card.{ Value => CValue, Two => CTwo, Three => CThree, Four => CFour }

object P54 extends App {

  import Poker._

  val answer = (for {
    line <- Resource.fromClasspath("P54.txt").lines()
    parsed = CardParser.parseHandPair(line) map { case (Strengths(s1), Strengths(s2)) => s1 gt s2 }
    result = parsed getOrElse false
    if result
  } yield ()) size

  println(answer)
}

object Poker {

  sealed trait Rank
  case object RoyalFlush extends Rank
  case object StraightFlush extends Rank
  case object FourOfAKind extends Rank
  case object FullHouse extends Rank
  case object Flush extends Rank
  case object Straight extends Rank
  case object ThreeOfAKind extends Rank
  case object TwoPairs extends Rank
  case object OnePair extends Rank
  case object HighCard extends Rank

  implicit lazy val RankOrder: Order[Rank] = Order.orderBy[Rank, Int] {
    case RoyalFlush => 10
    case StraightFlush => 9
    case FourOfAKind => 8
    case FullHouse => 7
    case Flush => 6
    case Straight => 5
    case ThreeOfAKind => 4
    case TwoPairs => 3
    case OnePair => 2
    case HighCard => 1
  }

  implicit lazy val CValueEnum: Enum[CValue] = new Enum[CValue] {
    def order(x: CValue, y: CValue) = Order.orderBy[CValue, Int] {
      case CTwo => 2
      case CThree => 3
      case CFour => 4
      case Five => 5
      case Six => 6
      case Seven => 7
      case Eight => 8
      case Nine => 9
      case Ten => 10
      case Jack => 11
      case Queen => 12
      case King => 13
      case Ace => 14
    }.order(x, y)

    def pred(a: CValue): CValue = a match {
      case CTwo => CTwo
      case CThree => CTwo
      case CFour => CThree
      case Five => CFour
      case Six => Five
      case Seven => Six
      case Eight => Seven
      case Nine => Eight
      case Ten => Nine
      case Jack => Ten
      case Queen => Jack
      case King => Queen
      case Ace => King
    }

    def succ(a: CValue): CValue = a match {
      case CTwo => CThree
      case CThree => CFour
      case CFour => Five
      case Five => Six
      case Six => Seven
      case Seven => Eight
      case Eight => Nine
      case Nine => Ten
      case Ten => Jack
      case Jack => Queen
      case Queen => King
      case King => Ace
      case Ace => Ace
    }
  }

  implicit val CValueOrdering: scala.math.Ordering[CValue] = implicitly[Order[CValue]].toScalaOrdering

  case class Strength(rank: Rank, values: List[CValue])

  object Strengths {
    def pairs(nums: List[Int])(hand: Hand): Option[List[CValue]] = {
      val group: List[(CValue, List[Card])] = hand.cards.toList.groupBy(_.value).toList
      (group.map { case (_, l) => l.size }.sorted ≟ nums) option {
        implicit val sort =
          Order.orderBy[(CValue, List[Card]), Int] { case (_, l) => l.size } |+|
        Order.orderBy[(CValue, List[Card]), CValue] { case (v, _) => v } |> (_.reverseOrder.toScalaOrdering)
        group.sorted.map(_._1)
      }
    }
    object HighCard {
      def unapply(hand: Hand): Option[List[CValue]] = pairs(1 :: 1 :: 1 :: 1 :: 1 :: Nil)(hand)
    }
    object OnePair {
      def unapply(hand: Hand): Option[List[CValue]] = pairs(1 :: 1 :: 1 :: 2 :: Nil)(hand)
    }
    object TwoPairs {
      def unapply(hand: Hand): Option[List[CValue]] = pairs(1 :: 2 :: 2 :: Nil)(hand)
    }
    object ThreeOfAKind {
      def unapply(hand: Hand): Option[List[CValue]] = pairs(1 :: 1 :: 3 :: Nil)(hand)
    }
    object Straight {
      def unapply(hand: Hand): Option[CValue] = HighCard.unapply(hand) >> {
        val sorted = hand.cards.toList.map(_.value).sorted
        sorted ≟ (sorted.head.from take 5 toList) option sorted.head
      }
    }
    object Flush {
      def unapply(hand: Hand): Option[List[CValue]] = (hand.cards.map(_.suit).size ≟ 1 option ()) >> HighCard.unapply(hand)
    }
    object FullHouse {
      def unapply(hand: Hand): Option[List[CValue]] = pairs(2 :: 3 :: Nil)(hand)
    }
    object FourOfAKind {
      def unapply(hand: Hand): Option[List[CValue]] = pairs(1 :: 4 :: Nil)(hand)
    }
    object StraightFlush {
      def unapply(hand: Hand): Option[Unit] = (hand.cards.map(_.value).toSet ≟ Set(Ten, Jack, Queen, King, Ace)) option ()
    }
    object RoyalFlush {
      def unapply(hand: Hand): Option[Unit] = StraightFlush.unapply(hand) >> (hand.cards.map(_.suit).toSet.size ≟ 1 option ())
    }

    def unapply(hand: Hand): Option[Strength] = hand match {
      case RoyalFlush(_) => Strength(Poker.RoyalFlush, Nil).some
      case StraightFlush(_) => Strength(Poker.StraightFlush, Nil).some
      case FourOfAKind(v) => Strength(Poker.FourOfAKind, v).some
      case FullHouse(v) => Strength(Poker.FullHouse, v).some
      case Flush(v) => Strength(Poker.Flush, v).some
      case Straight(v) => Strength(Poker.Straight, v :: Nil).some
      case ThreeOfAKind(v) => Strength(Poker.ThreeOfAKind, v).some
      case TwoPairs(v) => Strength(Poker.TwoPairs, v).some
      case OnePair(v) => Strength(Poker.OnePair, v).some
      case HighCard(v) => Strength(Poker.HighCard, v).some
      case _ => none
    }
  }

  implicit val StrengthOrder: Order[Strength] =
    Order.orderBy[Strength, Rank](_.rank) |+| Order.orderBy[Strength, List[CValue]](_.values)
}
