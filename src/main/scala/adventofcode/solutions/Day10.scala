package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day10 = Day(10) { (input, part) =>

  case class Vec(i: Int, j: Int):
    inline def +(that: Vec): Vec = Vec(i + that.i, j + that.j)

    inline def *(v: Int): Vec = Vec(i * v, j * v)
    def opposite: Vec = Vec(-i, -j)
  val around =
    for
      i <- -1 to 1
      j <- -1 to 1
      if (i.abs == 1) != (j.abs == 1)
    yield Vec(i, j)

  def connected(north: Boolean = false, east: Boolean = false, south: Boolean = false, west: Boolean = false): Set[Vec] =
    Set(
      north -> Vec(-1, 0),
      east -> Vec(0, 1),
      south -> Vec(1, 0),
      west -> Vec(0, -1)
    ).collect { case (true, vec) => vec }

  enum Tile:
    case PipeVertical
    case PipeHorizontal
    case BendNE
    case BendNW
    case BendSW
    case BendSE
    case Ground
    case Start

    def connections: Set[Vec] = this match
      case PipeVertical => connected(north = true, south = true)
      case PipeHorizontal => connected(west = true, east = true)
      case BendNE => connected(north = true, east = true)
      case BendNW => connected(north = true, west = true)
      case BendSW => connected(south = true, west = true)
      case BendSE => connected(south = true, east = true)
      case _ => connected()

  import Tile._

  val mapWithStart: IndexedSeq[IndexedSeq[Tile]] = input.toLines.map(_.map {
    case '|' => PipeVertical
    case '-' => PipeHorizontal
    case 'L' => BendNE
    case 'J' => BendNW
    case '7' => BendSW
    case 'F' => BendSE
    case '.' => Ground
    case 'S' => Start
  })

  def inside(vec: Vec): Boolean = mapWithStart.indices.contains(vec.i) && mapWithStart.head.indices.contains(vec.j)

  def start = mapWithStart.zipWithIndex.flatMap((row, i) => row.zipWithIndex.collect { case (Start, j) => Vec(i, j) }).head
  val startDirections = around.filter { direction =>
    val vec = start + direction
    if inside(vec) then
      val tile = mapWithStart(vec.i)(vec.j)
      tile.connections.contains(direction.opposite)
    else
      false
  }.toSet

  val startTile: Tile = Tile.values.find(tile => tile.connections == startDirections).head

  val map = mapWithStart.updated(start.i, mapWithStart(start.i).updated(start.j, startTile))

  def bfs(current: Set[Vec], total: Set[Vec], visited: Set[Vec]): Set[Vec] =
    val nextVisited = visited ++ current
    val nextPairsUnfiltered = current.flatMap { vec =>
      val connections = map(vec.i)(vec.j).connections
      connections.map(direction => (vec + direction) -> direction)
    }.toMap
    val next = nextPairsUnfiltered.keySet.diff(nextVisited)
    val nextTotal = total ++ nextPairsUnfiltered.flatMap { (v, d) =>
      val double = v * 2
      Seq(double, double + d.opposite)
    }
    if next.isEmpty then
      nextTotal
    else
      bfs(next, nextTotal, nextVisited)

  val loop = bfs(Set(start), Set(start * 2), Set.empty)

  part(1) = loop.size / 4

  val (doubleRows, doubleColumns) = (0 until 2 * mapWithStart.size - 1, 0 until 2 * mapWithStart.head.size - 1)
  def insideDouble(vec: Vec): Boolean = doubleRows.contains(vec.i) && doubleColumns.contains(vec.j)

  def fill(current: Set[Vec], visited: Set[Vec]): Set[Vec] =
    if current.nonEmpty then
      val nextVisited = visited ++ current
      val next = current.flatMap(vec => around.map(vec + _).filter(insideDouble)).diff(nextVisited).diff(loop)
      fill(next, nextVisited)
    else
      visited

  val initial = Seq(
    doubleRows.map(Vec(_, 0)),
    doubleRows.map(Vec(_, doubleColumns.size - 1)),
    doubleColumns.map(Vec(0, _)),
    doubleColumns.map(Vec(doubleRows.size - 1, _)),
  ).flatten.toSet.diff(loop)

  val area = fill(initial, Set.empty)

  part(2) = (
    for
      i <- doubleRows
      j <- doubleColumns
      vec = Vec(i, j)
      if i % 2 == 0 && j % 2 == 0
      if !area.contains(vec) && !loop.contains(vec)
    yield 1
  ).sum

}
