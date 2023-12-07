package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day07 = Day(7) { (input, part) =>

  val Strengths1 = "AKQJT98765432"
  val Strengths2 = "AKQT98765432J"

  case class Hand(cards: Seq[Char], original: Seq[Char], bid: Int):
    def strengths(o: Boolean): Seq[Int] = (if o then original else cards).map(card => (if o then Strengths2 else Strengths1).indexOf(card))
    def grouped: Seq[(Int, Int)] = strengths(false).sorted.groupBy(identity).view.mapValues(_.size).toSeq.sortBy((l, c) => (-c, l))
    def tpe: Int =
      val counts = grouped.map(_._2)
      Seq(
        counts == Seq(5),
        counts == Seq(4, 1),
        counts == Seq(3, 2),
        counts == Seq(3, 1, 1),
        counts == Seq(2, 2, 1),
        counts == Seq(2, 1, 1, 1),
        true
      ).indexOf(true)
    def replaced: Hand =
      val J = 'J'
      val label = grouped.map((l, _) => Strengths1(l)).find(_ != J).getOrElse(Strengths1.head)
      copy(cards = cards.map {
        case J => label
        case other => other
      })

  def ordering(o: Boolean): Ordering[Hand] =
    Ordering.by(hand => (hand.tpe, hand.strengths(true).foldLeft(0)((a, s) => a * Strengths1.length + s)))

  val hands = input.toLines.map(_.split(" ").toSeq).map {
    case Seq(cardsRaw, bid) =>
      val cards = cardsRaw.toSeq
      Hand(cards, cards, bid.toInt)
  }

  def compute(hands: Seq[Hand], o: Boolean): Int =
    hands.sorted(using ordering(o)).reverse.zipWithIndex.map((hand, i) => hand.bid * (i + 1)).sum

  part(1) = compute(hands, true)

  part(2) = compute(hands.map(_.replaced), false)

}
