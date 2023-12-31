package adventofcode.solutions

import adventofcode.Definitions.*

import scala.io.Source

@main def Day25 = Day(25) { (input, part) =>

  val components = input.toLines.flatMap {
    case s"$lhs: $rhs" => rhs.split(" ").toSeq.flatMap(other => Seq(lhs -> other, other -> lhs))
  }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap

  // (third party tool)

  val remove = Seq(
    "rmt" -> "nqh",
    "psj" -> "fdb",
    "trh" -> "ltn"
  ).flatMap((a, b) => Seq(a -> b, b -> a)).toSet

  def bfs(current: Set[String], history: Set[String]): Int =
    if current.nonEmpty then
      val newHistory = history ++ current
      val newCurrent = current.flatMap(k => components(k).map(k -> _).diff(remove).map(_._2).diff(newHistory))
      bfs(newCurrent, newHistory)
    else
      history.size

  val size = bfs(Set(components.keySet.head), Set.empty)

  part(1) = size * (components.size - size)

  part(2) = ""

}
