package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day02 = Day(2) { (input, part) =>

  case class Game(id: Int, rounds: Seq[Map[String, Int]])

  val games = input.toLines.map {
    case s"Game $i: $s" => Game(
      i.toInt,
      s.split("; ").map(_.split(", ").map(_.split(" ")).map(p => p.last -> p.head.toInt).toMap)
    )
  }

  val table = Map("red" -> 12, "green" -> 13, "blue" -> 14)
  part(1) = games.filter(g => g.rounds.forall(m => table.keys.forall(k => table(k) >= m.getOrElse(k, 0)))).map(_.id).sum

  part(2) = games
    .map(_.rounds.fold(Map.empty)((a, b) => (a.keys ++ b.keys).map(k => k -> Math.max(a.getOrElse(k, 0), b.getOrElse(k, 0))).toMap))
    .map(_.values.product).sum

}
