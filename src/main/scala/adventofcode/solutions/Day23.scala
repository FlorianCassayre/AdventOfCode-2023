package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day23 = Day(23) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    def unary_- : Vec = Vec(-i, -j)

  val directions =
    val range = -1 to 1
    for
      i <- range
      j <- range
      if (i == 0) != (j == 0)
    yield Vec(i, j)
  val directionsSet = directions.toSet

  val map = input.toLines.map(_.map {
    case '.' => Some(None)
    case '#' => None
    case '^' => Some(Some(Vec(-1, 0)))
    case '>' => Some(Some(Vec(0, 1)))
    case 'v' => Some(Some(Vec(1, 0)))
    case '<' => Some(Some(Vec(0, -1)))
  })

  val (start, end) = (Vec(0, 1), Vec(map.size - 1, map.head.size - 2))
  val startDirection = Vec(1, 0)

  def inBounds(point: Vec): Boolean =
    map.indices.contains(point.i) && map(point.i).indices.contains(point.j)

  def longestWithSlopes(positions: Set[(Vec, Vec)], i: Int): Int =
    if positions.nonEmpty then
      val newPositions = positions.flatMap((p, d0) =>
        (directionsSet - -d0)
          .map(d1 => (p + d1, d1))
          .filter((p, _) => inBounds(p))
          .filter { (p, d) =>
            val tile = map(p.i)(p.j)
            tile.exists(_.forall(_ == d))
          }
      )
      longestWithSlopes(newPositions, i + 1)
    else
      i - 1

  part(1) = longestWithSlopes(Set(start -> startDirection), 0)

  val intersectionsSet =
    val intersections =
      for
        i <- map.indices
        j <- map(i).indices
        p0 = Vec(i, j)
        if (directions.map(p0 + _) :+ p0).filter(inBounds).count(p1 => map(p1.i)(p1.j).nonEmpty) > 3
      yield p0
    intersections.toSet

  def reachableIntersections(current: Set[Vec], history: Map[Vec, Int], i: Int): Map[Vec, Int] =
    if current.nonEmpty then
      val newCurrent = current
        .flatMap(p => directions.map(p + _))
        .filter(inBounds)
        .filter(p => map(p.i)(p.j).nonEmpty)
        .diff(history.keySet)
      reachableIntersections(newCurrent.diff(intersectionsSet), history ++ newCurrent.map(_ -> i).toMap, i + 1)
    else
      history.view.filterKeys(intersectionsSet.contains).toMap

  val intersectionsGraph = intersectionsSet
    .map(intersection => intersection -> (reachableIntersections(Set(intersection), Map(intersection -> 0), 1) - intersection)).toMap

  val startEndIntersections = Seq(start, end).map(p => reachableIntersections(Set(p), Map(p -> 0), 0).head)
  val (startIntersection, endIntersection) = startEndIntersections.map(_._1) match
    case Seq(se, ee) => (se, ee)
  val initialCost = startEndIntersections.map(_._2 + 1).sum

  def longestIntersectionPath(current: Vec, history: Set[Vec], length: Int): Int =
    if current != endIntersection then
      val newHistory = history + current
      intersectionsGraph(current)
        .filter((v, _) => !newHistory.contains(v))
        .map((v, cost) => longestIntersectionPath(v, newHistory, length + cost))
        .maxOption.getOrElse(0)
    else
      length

  part(2) = longestIntersectionPath(startIntersection, Set.empty, initialCost)

}
