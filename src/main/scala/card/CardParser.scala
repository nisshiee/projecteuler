package projecteuler.card

import scala.util.parsing.combinator._

object CardParser extends RegexParsers {

  val suit: Parser[Suit] = """[CSDH]""".r ^^ {
    case "C" => Club
    case "S" => Spade
    case "D" => Diamond
    case "H" => Heart
  }
  val value: Parser[Value] = """[23456789TJQKA]""".r ^^ {
    case "2" => Two
    case "3" => Three
    case "4" => Four
    case "5" => Five
    case "6" => Six
    case "7" => Seven
    case "8" => Eight
    case "9" => Nine
    case "T" => Ten
    case "J" => Jack
    case "Q" => Queen
    case "K" => King
    case "A" => Ace
  }
  val card: Parser[Card] = value ~ suit ^^ { case v ~ s => Card(s, v) }

  val hand: Parser[Hand] = card ~ card ~ card ~ card ~ card ^^ {
    case c1 ~ c2 ~ c3 ~ c4 ~ c5 => Hand(Set(c1, c2, c3, c4, c5))
  }

  val handPair: Parser[(Hand, Hand)] = hand ~ hand ^^ {
    case h1 ~ h2 => (h1, h2)
  }

  def parseHandPair(line: String): ParseResult[(Hand, Hand)] = parseAll(handPair, line)
}
