package adventofcode.solutions

import adventofcode.Definitions.*

import scala.util.Random

@main def Day25 = Day(25) { (input, part) =>

  val components = input.toLines.flatMap {
    case s"$lhs: $rhs" => rhs.split(" ").toSeq.flatMap(other => Seq(lhs -> other, other -> lhs))
  }.groupBy(_._1).view.mapValues(_.map(_._2).toSeq).toMap

  val vertices = components.keySet.toIndexedSeq

  def randomPath(current: String, history: Map[String, String], random: Random): Set[(String, String)] =
    val nexts = components(current).filterNot(history.contains)
    if nexts.nonEmpty then
      val next = nexts(random.nextInt(nexts.size))
      randomPath(next, history + (current -> next) + (next -> current), random)
    else
      history.toSet.filter(_ < _)

  val remove = (0 until 1000).map(new Random(_))
    .map(random => randomPath(vertices(random.nextInt(vertices.size)), Map.empty, random))
    .sortBy(-_.size)
    .take(100)
    .foldLeft(Map.empty[(String, String), Int])((counts, add) =>
      add.foldLeft(counts)((counts, key) => counts + (key -> (counts.getOrElse(key, 0) + 1)))
    ).toSeq.sortBy((_, count) => -count).map((t, _) => t).take(3).flatMap(t => Seq(t, t.swap))

  def bfs(current: Set[String], history: Set[String]): Int =
    if current.nonEmpty then
      val newHistory = history ++ current
      val newCurrent = current.flatMap(k => components(k).map(k -> _).diff(remove).map(_._2).filterNot(newHistory.contains))
      bfs(newCurrent, newHistory)
    else
      history.size

  val size = bfs(Set(components.keySet.head), Set.empty)

  part(1) = size * (components.size - size)

  part(2) = ""

}
