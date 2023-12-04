package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day04 = Day(4) { (input, part) =>

  case class Card(id: Int, winning: Set[Int], self: Set[Int])

  val cards = input.toLines.map {
    case s"Card $n: $left | $right" =>
      def parse(s: String) = s.trim.split(" +").map(_.trim.toInt).toSet
      Card(n.trim.toInt, parse(left), parse(right))
  }

  val winning = cards.map(c => c.id -> c.winning.intersect(c.self).size).toMap

  part(1) = winning.values.filter(_ > 0).map(1 << _ - 1).sum

  part(2) = cards.zipWithIndex.foldLeft(cards.map(_.id -> 1).toMap) {
    case (counts, (card, i)) => cards
      .view.drop(i + 1).take(winning(card.id))
      .foldLeft(counts)((acc, e) => acc + (e.id -> (counts(card.id) + acc(e.id))))
  }.values.sum

}
